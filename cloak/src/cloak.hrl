-include("sandbox_pb.hrl").
-include("user_data.hrl").
-include("cloak_logging.hrl").

-include_lib("erlang_common/include/debug_helpers.hrl").

-define(JOB_EXECUTION_ERROR, <<"__AC_INTERNAL_ERROR__">>).

-define(WRAP(Q), fun() -> Q end).

% Instead of mocking the db job queue for tests,
% we completely drop it here.
-ifdef(TEST).
-define(QUEUED_WRITE(_Node, Q), Q).
-define(QUEUED_WRITE(Q), Q).
-define(QUEUED_GENERIC(Q), Q).
-define(QUEUED_STREAMING(Q), Q).
-else.
-define(QUEUED_WRITE(Node, Q), db_job:write(Node, ?WRAP(Q))).
-define(QUEUED_WRITE(Q), ?QUEUED_WRITE(node(), Q)).
-define(QUEUED_GENERIC(Q), db_job:generic(?WRAP(Q))).
-define(QUEUED_STREAMING(Q), db_job:streaming(?WRAP(Q))).
-endif.

-type user_id() :: string().
-type task_id() :: binary().
-type task_type() :: batch | streaming | periodic.
-type index() :: string().
-type analyst() :: pos_integer().
-type payload_identifier() :: binary().
-type code_id() :: binary().
-type code() :: binary().

%% We are currently operating with a ring size of 256 partitions, for which byte() is a good
%% type. This type does need to be adjusted when we grow the ring though!
-type partition_id() :: byte().
-type analyst_table_type() :: user | lookup | system.
-record(partition_table, {
  analyst_id :: analyst(),
  table_type :: analyst_table_type(),
  table_name :: table_name(),
  partition_id :: partition_id()
}).

%% Task specification types
-type prefetch_table_spec() :: [
  {table, binary()} |
  {user_rows, pos_integer() | undefined | null} |
  {time_limit, pos_integer() | undefined | null} |
  {where, db_query_builder:filter_spec()} |
  {columns, [binary()]}
].
-type prefetch_spec() :: [prefetch_table_spec()].
-type task_data() :: [{table_name(), [column_name()], [column_name()], [{supported_sql_data(), supported_sql_data()}]}].

-record(task, {
  task_id :: task_id(),
  type = batch :: task_type(),
  report_interval :: non_neg_integer() | undefined,
  period :: erlcron:run_when() | undefined,
  analyst_id :: analyst(),
  prefetch :: prefetch_spec(),
  code :: binary(),
  libraries = [] :: [{binary(), binary()}],
  source_ip :: string(),
  timestamp :: integer() | undefined,
  return_token :: return_token(),
  user_expire_interval :: undefined | pos_integer(),
  progress_handle :: binary(),
  progress_manager :: pid()
}).

-type noise_function() :: fun((integer()) -> integer()).

-type generated_range() :: {From :: integer() | neg_infinite, To :: integer() | infinite, Count :: non_neg_integer()}.

-type property_label() :: binary().
-type property_string() :: binary() | undefined.
-type job_accumulator() :: binary() | undefined.

-record(property, {
  label :: property_label(),
  value :: property_string()
}).

-record(job_response, {
  user_id :: user_id(),
  analyst_id :: analyst(),
  task_id :: task_id(),
  properties :: [#property{}],
  accumulator :: job_accumulator()
}).

-record(index_change, {
  user_id :: user_id(),
  analyst_id :: analyst(),
  index :: index(),
  change :: add | remove
}).

-record(bucket_label, {
  label :: property_label(),
  value :: property_string()
}).

-record(bucket_report, {
  label :: #bucket_label{},
  count :: non_neg_integer(),
  noisy_count :: number(),
  users_hash :: binary(),
  %% The size of the standard deviation used, to give the analyst a sense of the noise in the result
  noise_sd :: number()
}).

-type bucket_report_noisy_count() :: {new_report, integer()} | {previous_report, integer()} |
    % the following values are used internally by the anonymizer module
    {add_noise, non_neg_integer()} | {add_noise_check_error, non_neg_integer(), number(), integer()}.

-type previous_bucket_count() :: none | integer().

%% This record contains the parameters for the anonymizer.
-record(anonymizer_params, {
  %% The minimum number of users that must be in a bucket to get reported.
  absolute_lower_bound :: pos_integer(),

  %% After adding noise by with sigma sigma_soft_lower_bound to the number of users in a bucket this noisy count has
  %% to be greater than soft_lower_bound to get reported.
  soft_lower_bound :: pos_integer(),
  sigma_soft_lower_bound :: float(),

  %% target_error: The target error for which the anonymization engine will add noise to the results.
  target_error :: float(),

  %% Bounds for the noise added to anonymized results.
  %% min_sigma is the abosolute minimum standard deviation used, whereas max_sigma
  %% is more of a soft bound. Exceeding max_sigma will hurt usability, but not reduce the privacy
  %% protection provided by the system.
  min_sigma :: float(),
  max_sigma :: float(),

  %% The anonymized results contain a layer of noise that is constant and unique to
  %% the bucket. The noise is normal with a certain standard deviation.
  constant_noise_sd :: float()
}).

-type do_send() :: false | send_result().

-type send_result() :: {true, return_token(), #anonymizer_params{}}.
-type return_token() :: {result_format(), result_destination()}.
-type result_format() :: protobuf | json.
-type result_destination() :: {url, send_auth_token(), send_return_url()} | {process, pid()}.
-type send_auth_token() :: string().
-type send_return_url() :: string().

-type sql_date() :: {2014..3000, 1..12, 1..31}.
-type sql_time() :: {0..24, 0..59, 0..59 | float()}.
-type sql_timestamp() :: {sql_date(), sql_time()}.

-record(user_table, {
  analyst_id :: analyst(),
  name :: table_name(),
  row_expiry :: undefined | pos_integer()
}).

%% Helper macro for specifying cron intervals
-define(EVERY_N_SEC(Sec), {daily, {every, {Sec, sec}, {between, {0, 0, 0}, {23, 59, 59}}}}).

-ifndef(TEST).
%% The ?MIN_BATCH_SIZE setting provides the lower limit for the batch size. The actual batch size varies with
%% the number of columns requested. See "job_data_streamer:batch_size()" for more information.
-define(MIN_BATCH_SIZE, 30000).
-else.
%% For tests, we use a smaller size to shorten the test duration
-define(MIN_BATCH_SIZE, 200).
-endif.

%% The absolute count above which we consider that buckets will not be LCF-ed
%% Since LCF is not deterministic, we reject all buckets where count > threshold + 4 sigma, since it is
%% highly unlikely that such buckets will be rejected. This leaves a possibility that we won't have some
%% users for rejected buckets, but this will only skew the reported LCF tail count, and not compromise
%% the anonymization.
-define(LCF_CERTAINTY_THRESHOLD,
      (cloak_conf:get_val(noise, soft_lower_bound) + 4 * cloak_conf:get_val(noise, sigma_soft_lower_bound))
    ).

%% Special labels
-define(AIRCLOAK_LABEL, <<"aircloak">>).
-define(LCF_TAIL_VALUE, <<"lcf_tail">>).
