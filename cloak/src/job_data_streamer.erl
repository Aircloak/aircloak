%% @doc Implements streaming of prefetch data to jobs. This module will load the data in batches as
%%      to avoid exhaustion of memory for the cloak when processing large amounts of data.
-module(job_data_streamer).

%% API
-export([
  create_job_inputs/2,
  map_table_data_to_job_inputs/3,
  get_next_batch/4,
  batch_size/1
]).

-include("cloak.hrl").
-include("sandbox_pb.hrl").

%% The type of the data fields received from or sent to the database.
-type field_type() :: supported_sql_data() | datetime().

%% This record holds information required to stream data from a table.
%% The record is created before the job is scheduled by querying the database for aggregate information
%% and then, during job processing, the data is loaded batch by batch.
%% The total number of rows is obtained first and the timestamp batch query interval is computed
%% to result in an average row count of 0.75 * "batch_size()". The row density is, of course, not constant
%% over the entire timestamp range, so sometimes we have less rows, sometimes more, but never
%% less than 1 or more than "batch_size()".
%% We specify a timestamp range when loading each batch so that we speed the row lookup by using
%% the index and reducing the amount of sorted data.
-record(table_stream_state, {
  table_name :: binary(), % the table name that holds the data
  last_timestamp :: non_neg_integer(), % the timestamp of data from which the next batch starts
  min_timestamp :: non_neg_integer(), % the minimum timestamp of the data in the stream
  max_timestamp :: non_neg_integer(), % the maximum timestamp of the data in the stream
  timestamp_step :: pos_integer(), % the timestamp inteval size used when loading a batch
  filter :: {iodata(), [field_type()]}, % the SQL query filter for the table data
  columns :: [binary()] % the table columns we send to the jobs
}).

%% This record holds the table data for a job. We use this when we already have the data loaded
%% in memory and we want to send it all at once to the job.
-record(table_data, {
  columns :: [binary()],
  rows :: [#tabledatapb_row{}]
}).

%% The input for a job is a list of pairs consisting of the table name and either a #table_data{} (when
%% we have all the data loaded in memory) or a #table_stream_state{} (when we stream the data batch by batch).
-type job_input() :: [{binary(), #table_data{} | #table_stream_state{}}]. % {table name, table data or table stream}

-export_type([
  job_input/0
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc The batch_size call returns the maximum number of rows in a single batch from a table when
%%      streaming data to a task's jobs. The batch size can vary depending on the number of columns selected.
%%      Warning: we can't have more than "batch_size()" rows with the same timestamp in a table;
%%      in case we do, some of them will have to be dropped from the batch.
-spec batch_size(pos_integer()) -> pos_integer().
batch_size(ColumnsCount) when is_integer(ColumnsCount), ColumnsCount >= 0 ->
  ?MIN_BATCH_SIZE * (7 - min(ColumnsCount div 3, 6)).

%% @doc This creates the initial data stream states, from the task prefetch specification,
%%      for all relevant users.
-spec create_job_inputs(analyst(), prefetch_spec()) -> [{user_id(), job_input()}].
create_job_inputs(AnalystId, PrefetchSpec) ->
  UserTableStreams = [create_per_user_table_streams(AnalystId, QuerySpec) || QuerySpec <- PrefetchSpec],
  group_table_streams(lists:flatten(UserTableStreams)).

%% @doc Handles the get_next_batch sandbox calls.
%%      We first lookup the data state for the specified table and then read a new batch from it.
%%      If the ResetStream value is set to true, we restart streaming from the beggining of the data.
-spec get_next_batch(user_id(), binary(), job_input(), boolean()) -> {job_input(), #tabledatapb{} | undefined}.
get_next_batch(UserId, TableName, JobInput, ResetStream) ->
  case lists:keyfind(TableName, 1, JobInput) of
    false ->
      % this can occur either when the table name is wrong or when user has no data in this table
      % we have no way of correctly identifying the reason currently
      EmptyBatch = #tabledatapb{columns = [], rows = [], complete = true},
      {JobInput, EmptyBatch};
    {TableName, TableInput} ->
      {NewTableInput, Batch} = read_from_table(UserId, TableInput, ResetStream),
      NewJobInput = lists:keyreplace(TableName, 1, JobInput, {TableName, NewTableInput}),
      {NewJobInput, Batch}
  end.

%% @doc Maps in memory table data to the inputs for a task's jobs.
%%      The table data rows need to have the following format: {ac_user_id, ac_created_at, column fields ...}.
-spec map_table_data_to_job_inputs(binary(), [binary()], [{}]) -> [{user_id(), job_input()}].
map_table_data_to_job_inputs(TableName, TableColumns, TableData) ->
  % we use foldr here in order to preserve rows order
  UserDataMap = lists:foldr(fun (TableRow, UserDataMap) ->
        [UserId, _AcCreatedAt | Fields] = TableRow,
        NewUserRow = create_input_table_data_row(Fields),
        dict:update(UserId, fun(OldUserRows) ->
              [NewUserRow | OldUserRows]
            end, [NewUserRow], UserDataMap)
      end, dict:new(), TableData),
  [{UserId, [{TableName, #table_data{columns = TableColumns, rows = Rows}}]} ||
      {UserId, Rows} <- dict:to_list(UserDataMap)].


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% Reads data from a prefetch table and returns the next batch.
-spec read_from_table(user_id(), #table_data{} | #table_stream_state{}, boolean()) ->
    {#table_data{} | #table_stream_state{}, #tabledatapb{}}.
read_from_table(_UserId, #table_data{} = TableData, false) ->
  % this should never happen in normal circumstances (all data is returned in the first batch)
  EmptyBatch = #tabledatapb{columns = [], rows = [], complete = true},
  {TableData, EmptyBatch};
read_from_table(_UserId, #table_data{columns = Columns, rows = Rows} = TableData, true) ->
  Batch = #tabledatapb{columns = Columns, rows = Rows, complete = true},
  {TableData, Batch};
read_from_table(_UserId, #table_stream_state{last_timestamp = LastTimestamp,
      max_timestamp = MaxTimestamp} = TableStream, false) when LastTimestamp > MaxTimestamp ->
  % this should never happen in normal circumstances (data streaming has finished, needs to be reset)
  EmptyBatch = #tabledatapb{columns = [], rows = [], complete = true},
  {TableStream, EmptyBatch};
read_from_table(UserId, #table_stream_state{min_timestamp = MinTimestamp} = TableStream, true) ->
  read_from_table(UserId, TableStream#table_stream_state{last_timestamp = MinTimestamp}, false);
read_from_table(UserId, TableStream, false) ->
  % unpack the stream state
  Columns = TableStream#table_stream_state.columns,
  {FilterQuery, FilterParams} = TableStream#table_stream_state.filter,
  TableName = TableStream#table_stream_state.table_name,
  LastTimestamp = TableStream#table_stream_state.last_timestamp,
  TimestampStep = TableStream#table_stream_state.timestamp_step,
  MaxTimestamp = TableStream#table_stream_state.max_timestamp,
  ParamCount = length(FilterParams),
  BatchSize = batch_size(length(Columns)),
  % create the query for the next batch
  QuotedColumns = [[$,, $", Column, $"] || Column <- Columns],
  BatchQuery = [<<"SELECT ac_created_at">>, QuotedColumns, <<" FROM ">>, TableName,
      <<" WHERE (">>, FilterQuery, <<") AND ac_user_id = ">>, sql_util:param_placeholder(ParamCount + 1),
      <<" AND ac_created_at BETWEEN ">>, sql_util:param_placeholder(ParamCount + 2), <<" AND ">>,
      sql_util:param_placeholder(ParamCount + 3), <<" ORDER BY ac_created_at ASC LIMIT ">>,
      list_to_binary(integer_to_list(BatchSize))],
  % read next batch from database
  {Count, SelectedRows, SelectedMaxTimestamp} = load_next_batch(BatchQuery, UserId, LastTimestamp, TimestampStep, FilterParams),
  % if we got a full batch, we need to strip last timestamp as it might be truncated
  {Rows, NewLastTimestamp} = case Count of
    BatchSize -> remove_last_timestamp_entries(UserId, TableName, SelectedRows);
    _ -> {SelectedRows, SelectedMaxTimestamp}
  end,
  % convert the rows to the job input format
  InputRows = [begin
        [_AcCreatedAt | InputFields] = tuple_to_list(Row),
        create_input_table_data_row(InputFields)
      end || Row <- Rows],
  % update stream state and return data
  Complete = NewLastTimestamp >= MaxTimestamp,
  Batch = #tabledatapb{columns = Columns, rows = InputRows, complete = Complete},
  {TableStream#table_stream_state{last_timestamp = NewLastTimestamp + 1}, Batch}.

%% Loads the next batch from the database and returns the data and the last timestamp of the loaded data.
-spec load_next_batch(iodata(), user_id(), non_neg_integer(), pos_integer(), [field_type()]) ->
    {non_neg_integer(), [{field_type()}], pos_integer()}.
load_next_batch(Query, UserId, StartTimestamp, TimestampStep, FilterParams) ->
  EndTimestamp = StartTimestamp + TimestampStep,
  Params = FilterParams ++ [UserId, cloak_util:int_to_datetime(StartTimestamp),
      cloak_util:int_to_datetime(EndTimestamp)],
  case execute_select_query({Query, Params}) of
    {0, []} -> % no results found in this range, go to the next one
      load_next_batch(Query, UserId, EndTimestamp, TimestampStep, FilterParams);
    {Count, Data} ->
      {Count, Data, EndTimestamp}
  end.

%% Because we limit the returned data to "batch_size()" rows, we might get truncated data when we have multiple
%% rows with the same timestamp and they can't all fit in the batch. Because of this, we remove the rows with
%% the same last timestamp from the batch when it is full. If we have more than "batch_size()" rows with the
% same timestamp, we ignore the ones that can't fit in a batch.
-spec remove_last_timestamp_entries(user_id(), binary(), [{field_type()}]) ->
    {[{field_type()}], pos_integer()}.
remove_last_timestamp_entries(UserId, TableName, Rows) ->
  FirstCreatedAt = element(1, hd(Rows)),
  LastCreatedAt = element(1, lists:last(Rows)),
  LastTimestamp = cloak_util:datetime_to_int(LastCreatedAt),
  case FirstCreatedAt == LastCreatedAt of
    true ->
      % oops, this should not have happened, we inserted more than "batch_size()" rows with the same timestamp,
      % we issue a warning about this, we process only "batch_size" entries and ignore the rest
      ?WARNING("Selected rows do not fit in a single batch: Table ~p, User ~p, Timestamp ~p.",
          [TableName, UserId, FirstCreatedAt]),
      {Rows, LastTimestamp};
    false ->
      % we remove the rows with the same timestamp as the last one because we might have gotten
      % an incomplete upload batch in the selection
      {[Row || Row <- Rows, element(1, Row) /= LastCreatedAt], LastTimestamp - 1}
    end.

%% Creates the stream states for a single table prefetch specification.
-spec create_per_user_table_streams(analyst(), prefetch_spec()) ->
    [{user_id(), {binary(), #table_data{} | #table_stream_state{}}}].
create_per_user_table_streams(AnalystId, QuerySpec) ->
  {TableName, Columns, RowLimit, MinTimestamp, MaxTimestamp, Filter} = parse_data_query_spec(AnalystId, QuerySpec),
  FullTableName = analyst_tables:schema_and_table_name(AnalystId, user, TableName),
  SanitizedFullTableName = sql_util:sanitize_db_object(FullTableName),
  MetadataQuery = build_results_metadata_query(SanitizedFullTableName,
      RowLimit, MinTimestamp, MaxTimestamp, Filter),
  {_Count, ResultsMetadatas} = execute_select_query(MetadataQuery),
  [parse_results_metadata(TableName, SanitizedFullTableName, Columns, Filter, ResultsMetadata) ||
      ResultsMetadata <- ResultsMetadatas].

%% Parses a data query specification and extract the information needed to create the initial stream state.
parse_data_query_spec(AnalystId, QuerySpec) ->
  TableName = proplists:get_value(table, QuerySpec, null),
  true = TableName /= null, % this field is required
  RowLimit = proplists:get_value(user_rows, QuerySpec, null),
  MaxTimestamp = cloak_util:timestamp_to_int(os:timestamp()),
  MinTimestamp = case proplists:get_value(time_limit, QuerySpec, null) of
      null -> 0;
      TimeLimit when is_integer(TimeLimit) andalso TimeLimit > 0 -> MaxTimestamp - TimeLimit * 1000000
    end,
  Filter = db_query_builder:build_filter(proplists:get_value(where, QuerySpec, [])),
  MasterTableName = analyst_tables:schema_and_table_name(AnalystId, user, TableName),
  AircloakColumns = [<<"ac_user_id">>, <<"ac_created_at">>],
  TableColumns = cloak_db_def:table_columns(MasterTableName) -- AircloakColumns,
  Columns = case proplists:get_value(columns, QuerySpec, null) of
        null -> TableColumns; % not specified, select all columns
        SelectedColumns -> TableColumns -- (TableColumns -- SelectedColumns) % protect against SQL injection
      end,
  {TableName, Columns, RowLimit, MinTimestamp, MaxTimestamp, Filter}.

%% Builds the SQL query that will return aggregate information about the data in a table and
%% which is needed for streaming that data into batches.
build_results_metadata_query(TableName, null, MinTimestamp, MaxTimestamp, {FilterSQL, FilterParams}) ->
  ParamCount = length(FilterParams),
  MetadataSQL = [<<"SELECT ac_user_id, MIN(ac_created_at), MAX(ac_created_at), COUNT(ac_created_at) FROM ">>,
      TableName, <<" WHERE (">>, FilterSQL, <<") AND ac_created_at BETWEEN ">>,
      sql_util:param_placeholder(ParamCount + 1), <<" AND ">>, sql_util:param_placeholder(ParamCount + 2),
      <<" GROUP BY ac_user_id">>],
  MetadataParams = FilterParams ++ [cloak_util:int_to_datetime(MinTimestamp), cloak_util:int_to_datetime(MaxTimestamp)],
  {MetadataSQL, MetadataParams};
build_results_metadata_query(TableName, RowLimit, MinTimestamp, MaxTimestamp, {FilterSQL, FilterParams})
    when is_integer(RowLimit) andalso RowLimit > 0 ->
  % when a row limit is specified in the table prefetch, we need to partition the data so that
  %% we only get information about the interesting rows
  ParamCount = length(FilterParams),
  MetadataSQL = [<<"SELECT ac_user_id, MIN(ac_created_at), MAX(ac_created_at), COUNT(ac_created_at) FROM ("
      "SELECT ac_user_id, ac_created_at, ROW_NUMBER() OVER "
      "(PARTITION BY ac_user_id ORDER BY ac_created_at DESC) AS index FROM ">>, TableName, <<" WHERE (">>,
      FilterSQL, <<") AND ac_created_at BETWEEN ">>, sql_util:param_placeholder(ParamCount + 1), <<" AND ">>,
      sql_util:param_placeholder(ParamCount + 2), <<") numbered WHERE index <= ">>,
      sql_util:param_placeholder(ParamCount + 3), <<" GROUP BY ac_user_id">>],
  MetadataParams = FilterParams ++ [cloak_util:int_to_datetime(MinTimestamp),
      cloak_util:int_to_datetime(MaxTimestamp), RowLimit],
  {MetadataSQL, MetadataParams}.

%% Executes a SQL SELECT query with parameters and returns the resulting data.
execute_select_query({Query, Params}) ->
  {{select, Count}, Rows} = ?QUEUED_GENERIC(cloak_db:call(task_data_streamer,
        fun(Connection) ->
          sql_conn:extended_query(Query, Params, timer:minutes(15), Connection)
        end
      )),
  {Count, Rows}.

%% Creates the initial stream state from the metadata information about the streamed table data.
parse_results_metadata(TableName, SanitizedFullTableName, Columns, Filter, {UserId, MinDateTime, MaxDateTime, Count}) ->
  MinTimestamp = cloak_util:datetime_to_int(MinDateTime),
  MaxTimestamp = cloak_util:datetime_to_int(MaxDateTime),
  TimestampRange = MaxTimestamp - MinTimestamp,
  BatchSize = batch_size(length(Columns)),
  BatchCount = (Count div (BatchSize * 4 div 5)) + 1,
  TimestampStep = TimestampRange div BatchCount + 1,
  TableStream = #table_stream_state{table_name = SanitizedFullTableName, last_timestamp = MinTimestamp, min_timestamp = MinTimestamp,
      max_timestamp = MaxTimestamp, timestamp_step = TimestampStep, filter = Filter, columns = Columns},
  {UserId, {TableName, TableStream}}.

%% Groups the user - table_stream_state pairs into job_input structures.
group_table_streams(UserTableStreams) ->
  UserStreamsMap = lists:foldr(fun({UserId, TableStream}, UserStreamsMap) ->
        dict:update(UserId, fun(Streams) -> [TableStream | Streams] end, [TableStream], UserStreamsMap)
      end, dict:new(), UserTableStreams),
  dict:to_list(UserStreamsMap).

%% Packs the table row data into an job input row record.
-spec create_input_table_data_row([field_type()]) -> #tabledatapb_row{}.
create_input_table_data_row(Row) ->
  #tabledatapb_row{fields = [create_input_table_data_field(Value) || Value <- Row]}.

%% Packs the table field data into the job input data record.
-spec create_input_table_data_field(field_type()) -> #tabledatapb_field{}.
create_input_table_data_field(null) ->
  #tabledatapb_field{};
create_input_table_data_field(Number) when is_number(Number) ->
  #tabledatapb_field{number = Number};
create_input_table_data_field(String) when is_binary(String) orelse is_list(String) ->
  #tabledatapb_field{string = String};
create_input_table_data_field(Boolean) when is_boolean(Boolean) ->
  #tabledatapb_field{boolean = Boolean}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

create_row(Number) ->
  #tabledatapb_row{fields = [#tabledatapb_field{number = Number}]}.

test_data_mapping() ->
  Data = [
    [<<"user_1">>, <<0:64/integer>>, 1],
    [<<"user_2">>, <<0:64/integer>>, 2],
    [<<"user_1">>, <<0:64/integer>>, 3]
  ],
  JobInput = job_data_streamer:map_table_data_to_job_inputs(<<"foo">>, [<<"col">>], Data),
  ?assertEqual(JobInput, [
        {<<"user_2">>, [{<<"foo">>, #table_data{rows = [
            create_row(2)
          ], columns = [
            <<"col">>
          ]}}]},
        {<<"user_1">>, [{<<"foo">>, #table_data{rows = [
            create_row(1),
            create_row(3)
          ], columns = [
            <<"col">>
          ]}}]}
      ]).

setup_test_table() ->
  db_test:create_analyst_schema(1),
  db_test:create_analyst_table(1, "user_test",
      "\"Data Value\" integer, ac_user_id varchar(40), ac_created_at timestamp").

add_data(Count) ->
  Data = [{<<"user_test">>, [
    {columns, [<<"Data Value">>]},
    {data, [[Value rem 4] || Value <- lists:seq(1, Count)]}
  ]}],
  ok = db_test:add_users_data(1, [{<<"user-id">>, Data}], timer:seconds(5)).

fill_table() ->
  BatchSize = batch_size(1),
  ok = add_data(BatchSize div 2),
  ok = add_data(BatchSize),
  ok = add_data(BatchSize div 2).

get_row_value(#tabledatapb_row{fields = []}) ->
  0;
get_row_value(#tabledatapb_row{fields = [#tabledatapb_field{number = Value}]}) ->
  Value.

compute_rows_sum(_JobInput, true) ->
  0;
compute_rows_sum(JobInput, false) ->
  {NewJobInput, #tabledatapb{columns = Columns, rows = Rows, complete = Complete}} =
      get_next_batch(<<"user-id">>, <<"test">>, JobInput, false),
  ?assertEqual(true, Columns =:= [<<"Data Value">>] orelse Columns =:= []),
  Values = [get_row_value(Row) || Row <- Rows],
  BatchSum = lists:foldl(fun(Value, Sum) -> Value + Sum end, 0, Values),
  BatchSum + compute_rows_sum(NewJobInput, Complete).

test_data_streaming() ->
  setup_test_table(),
  fill_table(),
  PrefetchSpecAll = [[{table, <<"test">>}, {columns, [<<"Data Value">>]}]],
  BatchSize = batch_size(1),
  [{<<"user-id">>, JobInputAll}] = create_job_inputs(1, PrefetchSpecAll),
  ?assertEqual((0 + 1 + 2 + 3) * BatchSize * 2 div 4, compute_rows_sum(JobInputAll, false)),
  PrefetchSpecPartial = [[{table, <<"test">>}, {where, [{<<"$$Data Value">>, [{<<"$lte">>, 1}]}]}, {user_rows, BatchSize * 2}]],
  [{<<"user-id">>, JobInputPartial}] = create_job_inputs(1, PrefetchSpecPartial),
  ?assertEqual((0 + 1) * BatchSize * 2 div 4, compute_rows_sum(JobInputPartial, false)),
  PrefetchSpecEmpty = [[{table, <<"test">>}, {columns, []}]],
  [{<<"user-id">>, JobInputEmpty}] = create_job_inputs(1, PrefetchSpecEmpty),
  ?assertEqual(0, compute_rows_sum(JobInputEmpty, false)).

integration_test_() ->
  {setup,
    fun() -> db_test:setup() end,
    fun(_) -> db_test:teardown() end,
    [
      fun test_data_mapping/0,
      fun test_data_streaming/0
    ]
  }.

-endif.
