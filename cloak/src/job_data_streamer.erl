%% @doc Implements streaming of prefetch data to jobs. This module will load the data in batches as
%%      to avoid exhaustion of memory for the cloak when processing large amounts of data.
-module(job_data_streamer).

%% API
-export([
  create_job_inputs/1,
  get_next_batch/4,
  batch_size/1
]).

-include("cloak.hrl").
-include("sandbox_pb.hrl").

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
  source_id :: atom(),
  table_id :: atom(), % the table id that holds the data
  last_row_id :: integer(), % the timestamp of data from which the next batch starts
  min_row_id :: integer(), % the minimum timestamp of the data in the stream
  max_row_id :: integer(), % the maximum timestamp of the data in the stream
  row_id_step :: pos_integer(), % the timestamp inteval size used when loading a batch
  filter :: db_query_builder:query(), % the SQL query filter for the table data
  columns :: [binary()] % the table columns we send to the jobs
}).

%% The input for a job is a list of pairs consisting of the table name and a #table_stream_state{} record
-type job_input() :: [{binary(), #table_stream_state{}}]. % {table name, table stream}

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
-spec create_job_inputs(prefetch_spec()) -> [{user_id(), job_input()}].
create_job_inputs(PrefetchSpec) ->
  UserTableStreams = [create_per_user_table_streams(QuerySpec) || QuerySpec <- PrefetchSpec],
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


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% Reads data from a prefetch table and returns the next batch.
-spec read_from_table(user_id(), #table_stream_state{}, boolean()) -> {#table_stream_state{}, #tabledatapb{}}.
read_from_table(_UserId, #table_stream_state{last_row_id = LastRowId,
  	max_row_id = MaxRowId} = TableStream, false) when LastRowId > MaxRowId ->
  % this should never happen in normal circumstances (data streaming has finished, needs to be reset)
  EmptyBatch = #tabledatapb{columns = [], rows = [], complete = true},
  {TableStream, EmptyBatch};
read_from_table(UserId, #table_stream_state{min_row_id = MinRowId} = TableStream, true) ->
  read_from_table(UserId, TableStream#table_stream_state{last_row_id = MinRowId}, false);
read_from_table(UserId, TableStream, false) ->
  % unpack the stream state
  Columns = TableStream#table_stream_state.columns,
  Filter = TableStream#table_stream_state.filter,
  SourceId = TableStream#table_stream_state.source_id,
  TableId = TableStream#table_stream_state.table_id,
  LastRowId = TableStream#table_stream_state.last_row_id,
  RowIdStep = TableStream#table_stream_state.row_id_step,
  MaxRowId = TableStream#table_stream_state.max_row_id,
  BatchSize = batch_size(length(Columns)),
  % create the query for the next batch
  {DataRows, NewLastRowId} = load_next_batch(SourceId, TableId, UserId,
    LastRowId, RowIdStep, BatchSize, Columns, Filter),
  % convert the rows to the job input format
  InputRows = [create_input_table_data_row(DataRow) || DataRow <- DataRows],
  % update stream state and return data
  Complete = NewLastRowId >= MaxRowId,
  Batch = #tabledatapb{columns = Columns, rows = InputRows, complete = Complete},
  {TableStream#table_stream_state{last_row_id = NewLastRowId + 1}, Batch}.

%% Loads the next batch from the database and returns the data and the last row id of the loaded data.
-spec load_next_batch(atom(), atom(), user_id(), integer(),
  pos_integer(), pos_integer(), [binary()], db_query_builder:query()) -> {[[data_value()]], integer()}.
load_next_batch(SourceId, TableId, UserId, StartRowId, RowIdStep, BatchSize, Columns, Filter) ->
  EndRowId = StartRowId + RowIdStep,
  case 'Elixir.Cloak.DataSource':get_data_batch(SourceId, TableId, UserId,
      StartRowId, EndRowId, BatchSize, Columns, Filter) of
    {0, []} -> % no results found in this range, go to the next one
      load_next_batch(SourceId, TableId, UserId, EndRowId, RowIdStep, BatchSize, Columns, Filter);
    {BatchSize, DataRows} ->
      [LastRowId | _LastRowFields] = lists:last(DataRows),
      {DataRows, LastRowId};
    {_Count, DataRows} ->
      {DataRows, EndRowId}
  end.

%% Creates the stream states for a single table prefetch specification.
-spec create_per_user_table_streams(prefetch_spec()) -> [{user_id(), {binary(), #table_stream_state{}}}].
create_per_user_table_streams(QuerySpec) ->
  {TableName, SourceId, TableId, Columns, RowLimit, Filter} = parse_data_query_spec(QuerySpec),
  MetadataRows = 'Elixir.Cloak.DataSource':get_metadata(SourceId, TableId, Filter, RowLimit),
  [parse_metadata(TableName, SourceId, TableId, Columns, Filter, Metadata) || Metadata <- MetadataRows].

%% Parses a data query specification and extract the information needed to create the initial stream state.
-spec parse_data_query_spec(prefetch_table_spec()) ->
  {binary(), atom(), atom(), [binary()], non_neg_integer(), db_query_builder:query()}.
parse_data_query_spec(QuerySpec) ->
  TablePath = proplists:get_value(table, QuerySpec, null),
  true = TablePath /= null, % this field is required
  [SourceName, TableName] = binary:split(TablePath, <<"/">>, [global]),
  SourceId = binary_to_existing_atom(SourceName, utf8),
  TableId = binary_to_existing_atom(TableName, utf8),
  RowLimit = proplists:get_value(user_rows, QuerySpec, 0),
  Filter = db_query_builder:build_filter(proplists:get_value(where, QuerySpec, [])),
  TableColumns = 'Elixir.Cloak.DataSource':columns(SourceId, TableId),
  ColumnNames = [ColumnName || {ColumnName, _Type} <- TableColumns],
  SelectedColumns = case proplists:get_value(columns, QuerySpec, null) of
    null -> ColumnNames; % not specified, select all columns
    QueryColumns -> ColumnNames -- (ColumnNames -- QueryColumns) % protect against SQL injection
  end,
  {TablePath, SourceId, TableId, SelectedColumns, RowLimit, Filter}.

%% Creates the initial stream state from the metadata information about the streamed table data.
-spec parse_metadata(binary(), atom(), atom(), [binary()], db_query_builder:query(),
  {user_id(), integer(), integer(), non_neg_integer()}) -> {user_id(), {binary(), #table_stream_state{}}}.
parse_metadata(TableName, SourceId, TableId, Columns, Filter, {UserId, MinRowId, MaxRowId, Count}) ->
  RowIdRange = MaxRowId - MinRowId,
  BatchSize = batch_size(length(Columns)),
  BatchCount = (Count div (BatchSize * 4 div 5)) + 1,
  RowIdStep = RowIdRange div BatchCount + 1,
  TableStream = #table_stream_state{source_id = SourceId, table_id = TableId, last_row_id = MinRowId,
      min_row_id = MinRowId, max_row_id = MaxRowId, row_id_step = RowIdStep, filter = Filter, columns = Columns},
  {UserId, {TableName, TableStream}}.

%% Groups the user - table_stream_state pairs into job_input structures.
-spec group_table_streams([{user_id(), {binary(), #table_stream_state{}}}]) -> [{user_id(), job_input()}].
group_table_streams(UserTableStreams) ->
  UserStreamsMap = lists:foldr(fun({UserId, TableStream}, UserStreamsMap) ->
    dict:update(UserId, fun(Streams) -> [TableStream | Streams] end, [TableStream], UserStreamsMap)
  end, dict:new(), UserTableStreams),
  dict:to_list(UserStreamsMap).

%% Packs the table row data into an job input row record.
-spec create_input_table_data_row([data_value()]) -> #tabledatapb_row{}.
create_input_table_data_row([_RowId | Fields]) ->
  #tabledatapb_row{fields = [create_input_table_data_field(Field) || Field <- Fields]}.

%% Packs the table field data into the job input data record.
-spec create_input_table_data_field(data_value()) -> #tabledatapb_field{}.
create_input_table_data_field(nil) -> % data comes from Elixir code, which uses the atom 'nil' for not present
  #tabledatapb_field{};
create_input_table_data_field(Number) when is_number(Number) ->
  #tabledatapb_field{number = Number};
create_input_table_data_field(String) when is_binary(String) ->
  #tabledatapb_field{string = String};
create_input_table_data_field(Boolean) when is_boolean(Boolean) ->
  #tabledatapb_field{boolean = Boolean}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

setup_test_table() ->
  db_test:create_test_schema(),
  db_test:create_table(<<"test">>, <<"\"Data Value\" INTEGER">>).

add_data(Count) ->
  Data = [{<<"test">>, [
    {columns, [<<"Data Value">>]},
    {data, [[Value rem 4] || Value <- lists:seq(1, Count)]}
  ]}],
  ok = db_test:add_users_data([{<<"user-id">>, Data}]).

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
    get_next_batch(<<"user-id">>, db_test:table_path(<<"test">>), JobInput, false),
  ?assertEqual(true, Columns =:= [<<"Data Value">>] orelse Columns =:= []),
  Values = [get_row_value(Row) || Row <- Rows],
  BatchSum = lists:foldl(fun(Value, Sum) -> Value + Sum end, 0, Values),
  BatchSum + compute_rows_sum(NewJobInput, Complete).

test_data_streaming() ->
  setup_test_table(),
  fill_table(),
  TablePath = db_test:table_path(<<"test">>),
  PrefetchSpecAll = [[{table, TablePath}, {columns, [<<"Data Value">>]}]],
  BatchSize = batch_size(1),
  [{<<"user-id">>, JobInputAll}] = create_job_inputs(PrefetchSpecAll),
  ?assertEqual((0 + 1 + 2 + 3) * BatchSize * 2 div 4, compute_rows_sum(JobInputAll, false)),
  PrefetchSpecPartial = [[{table, TablePath}, {where, [{<<"$$Data Value">>, [{<<"$lte">>, 1}]}]}, {user_rows, BatchSize * 2}]],
  [{<<"user-id">>, JobInputPartial}] = create_job_inputs(PrefetchSpecPartial),
  ?assertEqual((0 + 1) * BatchSize * 2 div 4, compute_rows_sum(JobInputPartial, false)),
  PrefetchSpecEmpty = [[{table, TablePath}, {columns, []}]],
  [{<<"user-id">>, JobInputEmpty}] = create_job_inputs(PrefetchSpecEmpty),
  ?assertEqual(0, compute_rows_sum(JobInputEmpty, false)).

integration_test_() ->
  {setup,
    fun() -> db_test:setup() end,
    fun(_) -> ok end,
    fun test_data_streaming/0
  }.

-endif.
