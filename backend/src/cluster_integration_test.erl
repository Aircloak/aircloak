%% @doc Performs full scale integration tests of our infrastructure.
%%      The tests performed here are large scale and slow tests.
%%      They create clusters and builds, upload large amounts of data
%%      and run slow tasks.
%%
%%      The purpose of these tests is manifold. On one side we want to
%%      ensure that our system keeps on working over time, and be notified
%%      when it is not. On the other hand we also need raw number on the
%%      performance of standardized tasks in the context of our system.
%%      These performance metrics allow us to evaluate what our system
%%      performance is like, and to what extent it improves or worsens
%%      over time.
-module(cluster_integration_test).

%% API
-export([
  run/0
]).

%% Internal API needed by the maybe-monad style
%% `while_ok' function.
-export([
  get_analyst_id/1,
  create_build/1,
  create_cluster/1,
  create_table/1,
  upload_data/1,
  run_task/1
]).

-record(state, {
  db_connection :: pgsql_connection:pgsql_connection(),

  analyst_id :: undefined | pos_integer(),

  %% We keep a list of functions that are called
  %% at the end of the test, or when a test fails,
  %% in order to perform cleanup operations.
  cleanup_functions = [] :: [fun(() -> any())],

  % State for build
  build_id :: undefined | pos_integer(),

  % State for cluster
  cluster_id :: undefined | pos_integer(),
  cloaks = [] :: [binary()],

  % State for table
  table_id :: undefined | pos_integer(),
  table_name :: undefined | string(),

  % State for data
  users_uploaded :: undefined | non_neg_integer(),

  % Timing data
  timings = [] :: [{atom(), Seconds :: non_neg_integer()}]
}).

-type step_response() :: {ok, #state{}} | {error, any(), #state{}}.

-compile([{parse_transform, lager_transform}]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc This test will:
%%      - create a build (latest state of develop)
%%      - create a 3 node cluster using the build
%%      - create a table on the cluster
%%      - upload data to the table
%%      - run a task against the cluster
%%      - validate the response
%%      - destroy the cluster
%%      - destroy the build
%%
%%  It will also record the duration of the individual steps
%%  so that we have data to validate how long the different steps
%%  take, and how these times develop over time.
%%
%%  This is quite an exhaustive and slow test. It is therefore
%%  only scheduled to run once a week during the weekend.
-spec run() -> ok | error.
run() ->
  lager:info("Starting full cluster integration test"),
  {TestResult, FinalState} = while_ok(init_state(), [
        get_analyst_id,
        create_build,
        create_cluster,
        create_table,
        upload_data,
        run_task
      ]),
  flush_messages(),
  %% Send test results to rails for performance recordings
  TestIdentifier = <<"full_cluster_integration_test">>,
  RailsRequest = [
    {<<"action">>, <<"add_test_result">>},
    {<<"identifier">>, TestIdentifier}
  ],
  Success = TestResult == ok,
  ResultStructure = [
    {<<"timings">>, [
      [{cloak_util:binarify(Step), Time} || {Step, Time} <- FinalState#state.timings]
    ]},
    {<<"success">>, Success}
  ],
  FinalRailsRequest = [
    {<<"result">>, list_to_binary(mochijson2:encode(ResultStructure))} |
    RailsRequest
  ],
  flush_messages(),
  case rails_request(FinalRailsRequest, FinalState) of
    {ok, _Response} -> ok;
    {error, Reason, _State} ->
      lager:error("Failed at sending test results to frontend: ~p", [Reason]),
      error
  end.


%% -------------------------------------------------------------------
%% Get analyst header
%% -------------------------------------------------------------------

-spec get_analyst_id(#state{}) -> step_response().
get_analyst_id(State) ->
  case rails_request([{<<"action">>, <<"get_analyst_id">>}], State) of
    {ok, Response} ->
      {ok, State#state{
        analyst_id = ej:get({"analyst_id"}, Response)
      }};
    Other -> Other
  end.


%% -------------------------------------------------------------------
%% Creating build
%% -------------------------------------------------------------------

-spec create_build(#state{}) -> step_response().
create_build(State) ->
  ListenString = "build",
  listen_for(ListenString, State),
  lager:info("Requesting build creation"),
  case rails_request([{<<"action">>, <<"create_build">>}], State) of
    {ok, Response} ->
      BuildId = ej:get({"id"}, Response),
      UpdatedState = State#state{
        build_id = BuildId
      },
      CleanedState = case ej:get({"delete_later"}, Response) of
        true ->
          Cleanup = fun() -> destroy_build(BuildId, State) end,
          add_cleanup_step(Cleanup, UpdatedState);
        false ->
          UpdatedState
      end,
      case ej:get({"build_completed"}, Response) of
        true ->
          lager:info("Build was already complete."),
          {ok, CleanedState};
        false ->
          lager:info("Waiting for build ~p to complete", [BuildId]),
          %% After 2 hours we give up (2 * 60 * 60s)
          BinaryBuildId = cloak_util:binarify(BuildId),
          wait_for(ListenString, 7200, fun(ReportedBuildId) ->
                case ReportedBuildId =:= BinaryBuildId of
                  true ->
                    lager:info("Build ~p has completed", [BuildId]),
                    {ok, CleanedState};
                  false -> next
                end
              end, CleanedState)
      end;
    Other -> Other
  end.

destroy_build(BuildId, State) ->
  lager:info("Destroying build ~p", [BuildId]),
  Request = [{<<"action">>, <<"destroy_build">>},{<<"build_id">>, BuildId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      lager:warning("Cleaning the build failed with ~p", [Reason]);
    _ -> ok
  end.


%% -------------------------------------------------------------------
%% Creating cluster
%% -------------------------------------------------------------------

-spec create_cluster(#state{}) -> step_response().
create_cluster(#state{build_id=BuildId}=State) ->
  lager:info("Creating a cluster"),
  ListenString = "cluster_active",
  listen_for(ListenString, State),
  RailsRequest = [
    {<<"action">>, <<"create_cluster">>},
    {<<"build_id">>, BuildId}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      ClusterId = ej:get({"cluster_id"}, Response),
      StateWithCluster = State#state{
        cluster_id = ClusterId,
        cloaks = ej:get({"cloaks"}, Response)
      },
      Cleanup = fun() -> destroy_cluster(StateWithCluster) end,
      CleanedState = add_cleanup_step(Cleanup, StateWithCluster),
      lager:info("Waiting for cluster ~p to be setup", [ClusterId]),
      %% After 2 hours we give up (2 * 60 * 60s)
      BinaryClusterId = cloak_util:binarify(ClusterId),
      wait_for(ListenString, 7200, fun(ReportedClusterId) ->
            case ReportedClusterId =:= BinaryClusterId of
              true ->
                lager:info("Setup of cluster ~p has completed", [ClusterId]),
                wait_for_ring_migration(),
                {ok, CleanedState};
              false -> next
            end
          end, CleanedState);
    Other -> Other
  end.

wait_for_ring_migration() ->
  lager:info("Waiting for clusters to migrate"),
  case air_etcd:get("/settings/rails/global") of
    <<"true">> ->
      %% The clusters wait a time (15 minutes) before checking whether
      %% they should migrate, and then another 5 minutes to validate
      %% that no extra node was added. We wait 30 minutes here to ensure
      %% the cluster is in fact online and has migrated.
      timer:sleep(timer:minutes(30));
    _ ->
      % Local clusters are ready immediately
      ok
  end,
  lager:info("Assuming cluster has migrated successfully. Moving on").

destroy_cluster(#state{cluster_id=ClusterId}=State) ->
  lager:info("Destroying test cluster ~p", [ClusterId]),
  ListenString = "cluster_destroyed",
  listen_for(ListenString, State),
  Request = [{<<"action">>, <<"destroy_cluster">>},{<<"cluster_id">>, ClusterId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      lager:warning("Cleaning the build failed with ~p", [Reason]);
    _ ->
      %% After 2 hours we give up (2 * 60 * 60s)
      BinaryClusterId = cloak_util:binarify(ClusterId),
      lager:info("Waiting for cluster ~p to be destroyed", [ClusterId]),
      wait_for(ListenString, 7200, fun(ReportedClusterId) ->
            case ReportedClusterId =:= BinaryClusterId of
              true ->
                lager:info("Cluster ~p was destroyed", [ClusterId]),
                ok;
              false -> next
            end
          end, State)
  end.


%% -------------------------------------------------------------------
%% Creating task
%% -------------------------------------------------------------------

-spec create_table(#state{}) -> step_response().
create_table(#state{cluster_id=ClusterId}=State) ->
  lager:info("Creating database tables for test"),
  %% The TableJson and Migration is copied and pasted
  %% from what the web interface generates when you migrate a table
  TableName = "integration_test_table_" ++ integer_to_list(cloak_util:timestamp_to_int(now())),
  TableJson = cloak_util:binarify(
        "[{\"name\":\"int1\",\"constraints\":[],\"type\":\"bigint\"},
        {\"name\":\"int2\",\"constraints\":[],\"type\":\"bigint\"},
        {\"name\":\"double\",\"constraints\":[],\"type\":\"double\"},
        {\"name\":\"text\",\"constraints\":[],\"type\":\"text\"}]"),
  Migration = cloak_util:binarify(
        "{\"table_name\":\"" ++ TableName ++ "\",\"action\":\"create\",\"columns\":[
        {\"name\":\"int1\",\"constraints\":[],\"type\":\"bigint\"},
        {\"name\":\"int2\",\"constraints\":[],\"type\":\"bigint\"},
        {\"name\":\"double\",\"constraints\":[],\"type\":\"double\"},
        {\"name\":\"text\",\"constraints\":[],\"type\":\"text\"}]}"),
  RailsRequest = [
    {<<"action">>, <<"create_table">>},
    {<<"cluster_id">>, ClusterId},
    {<<"name">>, list_to_binary(TableName)},
    {<<"table_json">>, TableJson},
    {<<"migration">>, Migration}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      StateWithTable = State#state{
        table_id = ej:get({"table_id"}, Response),
        table_name = TableName
      },
      lager:info("Database tables created"),
      Cleanup = fun() -> destroy_table(StateWithTable) end,
      {ok, add_cleanup_step(Cleanup, StateWithTable)};
    Other -> Other
  end.

destroy_table(#state{table_id=TableId}=State) ->
  lager:info("Removing database table"),
  Request = [{<<"action">>, <<"destroy_table">>},{<<"table_id">>, TableId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      lager:warning("Removing test table ~p failed with ~p", [TableId, Reason]);
    _ -> ok
  end.


%% -------------------------------------------------------------------
%% Uploading data
%% -------------------------------------------------------------------

-spec upload_data(#state{}) -> step_response().
upload_data(State) ->
  lager:info("Creating key for data uploading"),
  RailsRequest = [
    {<<"action">>, <<"create_upload_key">>}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      lager:info("Key created"),
      KeyId = ej:get({"key_id"}, Response),
      Cleanup = fun() -> revoke_key(KeyId, State) end,
      CleanedState = add_cleanup_step(Cleanup, State),
      RawPem = ej:get({"key_pem"}, Response),
      [PrivateKey, Certificate] = public_key:pem_decode(RawPem),
      Password = ej:get({"password"}, Response),
      CertData = {PrivateKey, Certificate, Password},
      lager:info("Uploading user data"),
      create_and_upload_data(CertData, CleanedState);
    Other -> Other
  end.

revoke_key(KeyId, State) ->
  lager:info("Revoking test upload key"),
  Request = [{<<"action">>, <<"revoke_key">>},{<<"key_id">>, KeyId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      lager:warning("Revoking upload key with id ~p failed with ~p", [KeyId, Reason]);
    _ -> ok
  end.

-record(upload_state, {
  % How many rows on average we upload per user.
  mean_rows_per_user :: non_neg_integer(),
  table_name :: string(),
  % Full url for the cloak endpoint. Includes http:// and path
  cloak_url :: string(),
  % Options and headers used when connecting to cloak.
  ssl_options = [],
  headers = [],
  % The pid to which we return information about being done
  % with the upload when we have gotten that far
  return_pid :: pid()
}).

% We upload a random number of rows per user, but use a fixed
% random seed to get repeatable and reproducible tests.
create_and_upload_data(CertData, #state{table_name=TableName}=State) ->
  NumberOfUsers = binary_to_integer(air_etcd:get("/settings/air/integration_test/data/num_users")),
  MeanRowsPerUser = binary_to_integer(air_etcd:get("/settings/air/integration_test/data/mean_rows_per_user")),
  lager:info("Number of users: ~p, mean rows per user: ~p", [NumberOfUsers, MeanRowsPerUser]),
  GenericUploadState = #upload_state{
    mean_rows_per_user = MeanRowsPerUser,
    table_name = TableName,
    ssl_options = ssl_options(CertData),
    return_pid = self()
  },
  % We trick our random utility to seed itself and then reset the seed.
  % This way we ensure we don't fuck up our non-random seed by using
  % other aircloak functions, while at the same time controlling the seed
  % for reproducible tests.
  _ = random_util:uniform(),
  random:seed({0,0,0}),
  UploadStates = cloak_urls_and_headers(GenericUploadState, State),
  NumberOfUsersPerUploader = round(NumberOfUsers / length(UploadStates)),
  [spawn_link(fun() ->
        upload_data_for_user(NumberOfUsersPerUploader, UploadState, 0)
      end) || UploadState <- UploadStates],
  {ok, UsersUploaded} = wait_for_uploads_to_finish(length(UploadStates), 0),
  lager:info("Uploaded data for ~p users", [UsersUploaded]),
  {ok, State#state{users_uploaded=UsersUploaded}}.

cloak_urls_and_headers(UploadState, #state{cloaks=Cloaks, analyst_id=AnalystId}) ->
  case air_etcd:get("/settings/rails/global") of
    <<"true">> ->
      StateGen = fun(Cloak) ->
        UploadState#upload_state{
          cloak_url = "https://" ++ binary_to_list(Cloak) ++ "/bulk_insert",
          headers = []
        }
      end,
      [StateGen(Cloak) || Cloak <- Cloaks];
    <<"false">> ->
      StateGen = fun(Cloak) ->
        UploadState#upload_state{
          cloak_url = "http://" ++ binary_to_list(Cloak) ++ ":8098/bulk_insert",
          headers = [{"analyst", integer_to_list(AnalystId)}]
        }
      end,
      [StateGen(Cloak) || Cloak <- Cloaks]
  end.

wait_for_uploads_to_finish(0, Acc) -> {ok, Acc};
wait_for_uploads_to_finish(N, Acc) ->
  receive
    {done, UserCount} -> wait_for_uploads_to_finish(N-1, Acc+UserCount)
  end.

upload_data_for_user(0, UploadState, Acc) -> UploadState#upload_state.return_pid ! {done, Acc};
upload_data_for_user(RemainingUsers, UploadState, Acc) ->
  % Our table has the following columns:
  % - int1 : bigint (64bit)
  % - int2 : bigint (64bit)
  % - double : double (64bit)
  % - text : text (480 bytes)
  % Each row weighs in at 504 bytes
  RowsForUser = rows_for_user(UploadState#upload_state.mean_rows_per_user),
  OneIfUploaded = case RowsForUser > 0 of
    true ->
      UserNumStr = integer_to_binary(RemainingUsers),
      UserId = <<"integration-test-", UserNumStr/binary>>,
      TextForUser = base64:encode(crypto:rand_bytes(361)), % Roughly 480 after base64 encoding
      TableName = list_to_binary(UploadState#upload_state.table_name),
      Row = [
        {<<"int1">>, 1},
        {<<"int2">>, 2},
        {<<"double">>, 1.0},
        {<<"text">>, TextForUser}
      ],
      Payload = [
        {UserId, [
          {TableName, [Row || _ <- lists:seq(1, RowsForUser)]}
        ]}
      ],
      upload_to_server(UploadState, list_to_binary(mochijson2:encode(Payload))),
      1;
    false ->
      0
  end,
  upload_data_for_user(RemainingUsers - 1, UploadState, Acc+OneIfUploaded).

rows_for_user(MeanRowsPerUser) ->
  Rand1 = random_util:uniform(),
  Rand2 = random_util:uniform(),
  cloak_distributions:gauss(MeanRowsPerUser/4, MeanRowsPerUser, Rand1, Rand2).

upload_to_server(UploadState, Json) ->
  Request = {
    UploadState#upload_state.cloak_url,
    UploadState#upload_state.headers,
    "application/json",
    Json
  },
  {ok, {{_Version, Status, _StatusPhrase}, _Headers, Reply}} =
      httpc:request(post, Request, [{ssl, UploadState#upload_state.ssl_options}], []),
  case Status of
    200 -> ok;
    _ ->
      lager:info("Upload to ~p failed (~p): ~p",
          [UploadState#upload_state.cloak_url, Status, Reply])
  end.

ssl_options({PrivateKey, Certificate, Password}) ->
  [
    {verify, verify_none},
    {key, PrivateKey},
    {cert, Certificate},
    {password, Password}
  ].


%% -------------------------------------------------------------------
%% Run task
%% -------------------------------------------------------------------

-spec run_task(#state{}) -> step_response().
run_task(#state{cluster_id=ClusterId, table_name=TableName}=State) ->
  lager:info("Running task against cluster"),
  TaskCode = ["
    -- We loop through all rows of data,
    -- to stress the streaming mechanism
    row_count = 0
    for row in user_table(\"" ++ TableName ++ "\") do
      row_count = row_count + 1
    end
    report_property(\"Users\", \"count\")
    quantize_props = {min = 0, max = 10000, step=1}
    Aircloak.Distributions.quantize_property(\"Row count\", row_count, quantize_props)"
  ],
  Prefetch = "[{\"table\": \"" ++ TableName ++ "\"}]",
  RailsRequest = [
    {<<"action">>, <<"run_task">>},
    {<<"cluster_id">>, ClusterId},
    {<<"code">>, list_to_binary(TaskCode)},
    {<<"prefetch">>, list_to_binary(Prefetch)}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      lager:info("Task has been initiated. Waiting for results"),
      ChannelName = binary_to_list(ej:get({"channel_name"}, Response)),
      listen_for(ChannelName, State),
      %% We give the task 3 hours to complete (3 * 60 * 60s)
      case wait_for(ChannelName, 10800, fun(R) -> {ok, R} end, State) of
        {ok, BinaryResultId} ->
          ResultId = binary_to_integer(BinaryResultId),
          lager:info("Task finished. Result id: ~p", [ResultId]),
          validate_result(ResultId, State);
        {error, timeout} -> {error, timeout, State}
      end;
    Other -> Other
  end.

validate_result(ResultId, #state{users_uploaded=UserCount}=State) ->
  lager:info("Validating results"),
  JSONResult = air_db:call(fun(Connection) ->
        SQL = ["
          SELECT buckets_json
          FROM results
          WHERE results.id = $1"
        ],
        {{select, 1}, [{JSON}]} = pgsql_connection:extended_query(SQL, [ResultId], Connection),
        JSON
      end),
  Result = mochijson2:decode(JSONResult),
  NoisyUserCount = lists:foldl(fun(Row, Acc) ->
        Label = ej:get({"label"}, Row),
        Value = ej:get({"value"}, Row),
        case (Label =:= <<"Users">>) and (Value =:= <<"count">>) of
          true -> ej:get({"count"}, Row);
          false -> Acc
        end
      end, 0, Result),
  AllowedLowerBound = UserCount - 40, % 4SD of roughly SD=10
  AllowedUpperBound = UserCount + 40, % 4SD of roughly SD=10
  lager:info("Received noisy count of ~p. Expect it to lie within ~p and ~p. Real count: ~p",
      [NoisyUserCount, AllowedLowerBound, AllowedUpperBound, UserCount]),
  case (NoisyUserCount >= AllowedLowerBound) and (NoisyUserCount =< AllowedUpperBound) of
    true ->
      lager:info("Result is ok"),
      {ok, State};
    false ->
      lager:error("Integration test failed because the returned count was invalid"),
      {error, unexpected_task_result, State}
  end.


%% -------------------------------------------------------------------
%% Interacting with rails app
%% -------------------------------------------------------------------

rails_request(RequestPayload, State) ->
  JsonPayload = list_to_binary(mochijson2:encode(RequestPayload)),
  Url = binary_to_list(air_etcd:get("/service/frontend_local")) ++
      "/infrastructure-api/integration_tests",
  RailsRequest = {Url, [], "application/json", JsonPayload},
  case httpc:request(post, RailsRequest, [], []) of
    {ok, {_, _Headers, RawBody}} ->
      try mochijson2:decode(RawBody) of
        DecodedResponse ->
          case ej:get({"success"}, DecodedResponse) of
            true -> {ok, DecodedResponse};
            false -> {error, ej:get({"description"}, DecodedResponse), State}
          end
        catch error:_Reason ->
          {error, decoding_error, State}
      end;
    {error, Reason} ->
      lager:error("Rails integration tests backend failed with reason: ~p", [Reason]),
      {error, failed_connect, State}
  end.

listen_for(What, #state{db_connection=Connection}) ->
  {listen, []} = pgsql_connection:simple_query("LISTEN " ++ What, Connection),
  ok.

-spec wait_for(string(), pos_integer(), fun((binary()) -> next | any()), #state{}) ->
    {error, term()} | any().
wait_for(What, SecondsToWait, Fun, #state{db_connection=Connection}) ->
  EndTime = cloak_util:timestamp_to_epoch(now()) + SecondsToWait,
  WhatBinary = cloak_util:binarify(What),
  perform_wait_for(WhatBinary, EndTime, Fun, Connection).

perform_wait_for(WhatBinary, WaitUntil, Fun, Connection) ->
  Now = cloak_util:timestamp_to_epoch(now()),
  case WaitUntil < Now of
    true -> {error, timeout};
    false ->
      TimeDiffInMs = (WaitUntil - Now) * 1000,
      Result = receive
        {pgsql, Connection, {notification, _, WhatBinary, Data}} -> Fun(Data);
        _Other -> next
      after TimeDiffInMs ->
        {error, timeout}
      end,
      case Result of
        next -> perform_wait_for(WhatBinary, WaitUntil, Fun, Connection);
        Result -> Result
      end
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

flush_messages() ->
  receive
    Message ->
      lager:info("- ~p", [Message]),
      flush_messages()
  after 10 ->
      ok
  end.

-spec while_ok(#state{}, [atom()]) -> {ok | error, #state{}}.
while_ok(State, []) ->
  {ok, cleanup(State)};
while_ok(#state{timings=Timings}=State, [Function|Functions]) ->
  lager:info("Performing step: ~p", [Function]),
  StartTimeSeconds = cloak_util:timestamp_to_epoch(now()),
  try ?MODULE:Function(State) of
    {ok, NewState} ->
      EndTimeSeconds = cloak_util:timestamp_to_epoch(now()),
      Duration = EndTimeSeconds - StartTimeSeconds,
      NewStateWithTiming = NewState#state{timings=[{Function, Duration}|Timings]},
      lager:info("- ~p succeeded", [Function]),
      while_ok(NewStateWithTiming, Functions);
    {error, Reason, NewState} ->
      lager:warning("Test failed in step ~p with reason: ~p", [Function, Reason]),
      {error, cleanup(NewState)}
  catch
    ProblemType:ProblemReason ->
      lager:error("Test failed with ~p:~p", [ProblemType, ProblemReason]),
      {error, cleanup(State)}
  end.

%% @hidden
%% This function is only here to trick dialyzer.
%% If the database connection parameters are given directly
%% to the `pgsql_connection:open/1' command, it fails.
%% The cause of the failure is the `{async, pid()}` option.
%% It is unclear what is causing the problem, hence this
%% nasty workaround.
db_params() ->
  [{async, self()} | air_db:db_config()].

-spec init_state() -> #state{}.
init_state() ->
  Connection = pgsql_connection:open(db_params()),
  Cleanup = fun() ->
    pgsql_connection:simple_query("UNLISTEN *", Connection),
    pgsql_connection:close(Connection)
  end,
  add_cleanup_step(Cleanup, #state{db_connection = Connection}).

-spec cleanup(#state{}) -> #state{}.
cleanup(#state{cleanup_functions=CleanupFunctions}=State) ->
  [catch Function() || Function <- CleanupFunctions],
  State#state{cleanup_functions=[]}.

-spec add_cleanup_step(fun(() -> any()), #state{}) -> #state{}.
add_cleanup_step(Fun, #state{cleanup_functions=CleanupFunctions}=State) ->
  State#state{cleanup_functions=[Fun|CleanupFunctions]}.
