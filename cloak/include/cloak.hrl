-include("cloak_logging.hrl").

-include("debug_helpers.hrl").

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

-type user_id() :: binary() | integer().
-type task_id() :: binary().
-type index() :: string().

-type noise_function() :: fun((integer()) -> integer()).

-type generated_range() :: {From :: integer() | neg_infinite, To :: integer() | infinite, Count :: non_neg_integer()}.

-type job_accumulator() :: binary() | undefined.

-type property() :: [binary()] | binary().

-record(job_response, {
  user_id :: user_id(),
  task_id :: task_id(),
  properties :: [property()],
  accumulator :: job_accumulator()
}).

-record(bucket, {
  property :: property(),
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
  constant_noise_sd :: float(),

  %% Keeps state about which users have been low count filtered, and how many times.
  %% Allows us to later generate an anonymized property indicating how many user
  %% properties have been removed due to anonymization
  lcf_data :: 'Elixir.Cloak.LCFData':t()
}).

%% Helper macro for specifying cron intervals
-define(EVERY_N_SEC(Sec), {daily, {every, {Sec, sec}, {between, {0, 0, 0}, {23, 59, 59}}}}).

%% The absolute count above which we consider that buckets will not be LCF-ed
%% Since LCF is not deterministic, we reject all buckets where count > threshold + 4 sigma, since it is
%% highly unlikely that such buckets will be rejected. This leaves a possibility that we won't have some
%% users for rejected buckets, but this will only skew the reported LCF tail count, and not compromise
%% the anonymization.
-define(LCF_CERTAINTY_THRESHOLD,
      (cloak_conf:get_val(noise, soft_lower_bound) + 4 * cloak_conf:get_val(noise, sigma_soft_lower_bound))
    ).

%% Special labels
-define(LCF_TAIL_PROPERTY, <<"aircloak_lcf_tail">>).
