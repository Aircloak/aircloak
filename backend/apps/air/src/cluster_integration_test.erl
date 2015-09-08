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

-include("air.hrl").

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
  ?INFO("Starting full cluster integration test"),
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
      ?ERROR("Failed at sending test results to frontend: ~p", [Reason]),
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
  ?INFO("Requesting build creation"),
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
          ?INFO("Build was already complete."),
          {ok, CleanedState};
        false ->
          ?INFO("Waiting for build ~p to complete", [BuildId]),
          %% After 2 hours we give up (2 * 60 * 60s)
          BinaryBuildId = cloak_util:binarify(BuildId),
          wait_for(ListenString, 7200, fun(ReportedBuildId) ->
                case ReportedBuildId =:= BinaryBuildId of
                  true ->
                    ?INFO("Build ~p has completed", [BuildId]),
                    {ok, CleanedState};
                  false -> next
                end
              end, CleanedState)
      end;
    Other -> Other
  end.

destroy_build(BuildId, State) ->
  ?INFO("Destroying build ~p", [BuildId]),
  Request = [{<<"action">>, <<"destroy_build">>},{<<"build_id">>, BuildId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      ?WARNING("Cleaning the build failed with ~p", [Reason]);
    _ -> ok
  end.


%% -------------------------------------------------------------------
%% Creating cluster
%% -------------------------------------------------------------------

-spec create_cluster(#state{}) -> step_response().
create_cluster(#state{build_id=BuildId}=State) ->
  ?INFO("Creating a cluster"),
  ListenString = "cluster_active",
  listen_for(ListenString, State),
  RailsRequest = [
    {<<"action">>, <<"create_cluster">>},
    {<<"build_id">>, BuildId}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      ClusterId = ej:get({"cluster_id"}, Response),
      Cloaks = ej:get({"cloaks"}, Response),
      ?INFO("Cluster has the following cloaks: ~p", [Cloaks]),
      StateWithCluster = State#state{
        cluster_id = ClusterId,
        cloaks = Cloaks
      },
      Cleanup = fun() -> destroy_cluster(StateWithCluster) end,
      CleanedState = add_cleanup_step(Cleanup, StateWithCluster),
      ?INFO("Waiting for cluster ~p to be setup", [ClusterId]),
      %% After 2 hours we give up (2 * 60 * 60s)
      BinaryClusterId = cloak_util:binarify(ClusterId),
      wait_for(ListenString, 7200, fun(ReportedClusterId) ->
            case ReportedClusterId =:= BinaryClusterId of
              true ->
                ?INFO("Setup of cluster ~p has completed", [ClusterId]),
                {ok, CleanedState};
              false -> next
            end
          end, CleanedState);
    Other -> Other
  end.

destroy_cluster(#state{cluster_id=ClusterId}=State) ->
  ?INFO("Destroying test cluster ~p", [ClusterId]),
  ListenString = "cluster_destroyed",
  listen_for(ListenString, State),
  Request = [{<<"action">>, <<"destroy_cluster">>},{<<"cluster_id">>, ClusterId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      ?WARNING("Cleaning the build failed with ~p", [Reason]);
    _ ->
      %% After 2 hours we give up (2 * 60 * 60s)
      BinaryClusterId = cloak_util:binarify(ClusterId),
      ?INFO("Waiting for cluster ~p to be destroyed", [ClusterId]),
      wait_for(ListenString, 7200, fun(ReportedClusterId) ->
            case ReportedClusterId =:= BinaryClusterId of
              true ->
                ?INFO("Cluster ~p was destroyed", [ClusterId]),
                ok;
              false -> next
            end
          end, State)
  end.


%% -------------------------------------------------------------------
%% Creating table
%% -------------------------------------------------------------------

-spec create_table(#state{}) -> step_response().
create_table(#state{cluster_id=ClusterId}=State) ->
  ?INFO("Creating database tables for test"),
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
      ?INFO("Database tables created"),
      Cleanup = fun() -> destroy_table(StateWithTable) end,
      {ok, add_cleanup_step(Cleanup, StateWithTable)};
    Other -> Other
  end.

destroy_table(#state{table_id=TableId}=State) ->
  ?INFO("Removing database table"),
  Request = [{<<"action">>, <<"destroy_table">>},{<<"table_id">>, TableId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      ?WARNING("Removing test table ~p failed with ~p", [TableId, Reason]);
    _ -> ok
  end.


%% -------------------------------------------------------------------
%% Uploading data
%% -------------------------------------------------------------------

-spec upload_data(#state{}) -> step_response().
upload_data(State) ->
  ?INFO("Creating key for data uploading"),
  RailsRequest = [
    {<<"action">>, <<"create_upload_key">>}
  ],
  case rails_request(RailsRequest, State) of
    {ok, Response} ->
      ?INFO("Key created"),
      KeyId = ej:get({"key_id"}, Response),
      Cleanup = fun() -> revoke_key(KeyId, State) end,
      CleanedState = add_cleanup_step(Cleanup, State),
      RawPem = ej:get({"key_pem"}, Response),
      Password = ej:get({"password"}, Response),
      CertData = prepare_auth_material(RawPem, Password),
      ?INFO("Uploading user data"),
      create_and_upload_data(CertData, CleanedState);
    Other -> Other
  end.

prepare_auth_material(RawPem, Password) ->
  PasswordAsString = binary_to_list(Password),
  BaseName = "/tmp/upload_" ++ integer_to_list(cloak_util:timestamp_to_epoch(now())),
  RawPath = BaseName ++ ".raw",
  CertPath = BaseName ++ ".crt",
  KeyPath = BaseName ++ ".key",
  file:write_file(RawPath, RawPem),
  % We export the cert and passwordless key to individual
  % files to make it easier to use with httpc
  ExportKeyCommand = "openssl ec -inform PEM -outform PEM -in " ++ RawPath ++ " -passin pass:" ++ PasswordAsString ++ " -out " ++ KeyPath,
  ExportCertCommand = "openssl x509 -inform PEM -outform PEM -in " ++ RawPath ++ " -out " ++ CertPath,
  ?INFO("KeyPrep: ~p", [ExportKeyCommand]),
  ?INFO("CertPrep: ~p", [ExportCertCommand]),
  ?INFO("Key export result: ~p", [os:cmd(ExportKeyCommand)]),
  ?INFO("Certificate export: ~p", [os:cmd(ExportCertCommand)]),
  {KeyPath, CertPath}.

revoke_key(KeyId, State) ->
  ?INFO("Revoking test upload key"),
  Request = [{<<"action">>, <<"revoke_key">>},{<<"key_id">>, KeyId}],
  case rails_request(Request, State) of
    {error, Reason, _State} ->
      ?WARNING("Revoking upload key with id ~p failed with ~p", [KeyId, Reason]);
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
  % The goal of this stage is to upload a certain amount of data to our cloak cluster (n GB).
  % Given a fixed average number of rows per user, we need to upload a certain amount of users
  % to achieve our target data volume.
  % See `/etcd/etcd_values_prod` for the math behind the choice of number of
  % users and row per user.
  NumberOfUsers = binary_to_integer(air_etcd:get("/settings/air/integration_test/data/num_users")),
  MeanRowsPerUser = binary_to_integer(air_etcd:get("/settings/air/integration_test/data/mean_rows_per_user")),
  ?INFO("Number of users: ~p, mean rows per user: ~p", [NumberOfUsers, MeanRowsPerUser]),
  GenericUploadState = #upload_state{
    mean_rows_per_user = MeanRowsPerUser,
    table_name = TableName,
    ssl_options = ssl_options(CertData),
    return_pid = self()
  },
  UploadStates = cloak_urls_and_headers(GenericUploadState, State),
  % We create 50 parallel uploading sessions per cloak.
  % Experimentation has shown that, for the given upload sizes,
  % this yields near optimal upload speed for the current cloaks.
  NumUploadersPerCloak = 50,
  NumUploaders = length(UploadStates) * NumUploadersPerCloak,
  % We want to upload a fixed number of users (to reach our data volume goal),
  % and given the number of uploaders we have, the total number of users
  % to upload per uploader has to be adjusted.
  NumberOfUsersPerUploader = round(NumberOfUsers / NumUploaders),
  % In order to make debugging easier, we output progress markers
  % to the console as we go along. This way we know how far along
  % in the upload process we are, and whether we are still progressing.
  % The progress marker creates output for every 0.1% of the upload.
  % The max(?, 1) is dealing with the edge condition where a test uploads,
  % data for less than a 1000 users. The output results then end up
  % not being correct, but at the very least nothing crashes :)
  PerMille = max(round(NumberOfUsers / 1000), 1),
  UploadTallyPid = spawn(fun() -> tally_uploaded_rows(0, 0, PerMille) end),
  % We need each upload process to have a seeded random number generator.
  % We want the seed to be fixed in order to generate repeatable tests,
  % but we also want the seed to be unique per uploader.
  UploadersPlusSeed = lists:zip(UploadStates, lists:seq(1, length(UploadStates))),
  NumUploadersPerCloakSeq = lists:seq(1, NumUploadersPerCloak),
  UploadPids = [spawn_link(fun() ->
        % We trick our random utility to seed itself and then reset the seed.
        % This way we ensure we don't fuck up our non-random seed by using
        % other aircloak functions, while at the same time controlling the seed
        % for reproducible tests.
        _ = random_util:uniform(),
        random:seed({0,UploadNum,SeedInteger}),
        upload_data_for_user(NumberOfUsersPerUploader, UploadState, 0, UploadTallyPid)
      end) || {UploadState, SeedInteger} <- UploadersPlusSeed, UploadNum <- NumUploadersPerCloakSeq],
  case wait_for_uploads_to_finish(NumUploaders, 0, UploadPids) of
    {ok, UsersUploaded} ->
      ?INFO("Successfully uploaded data for ~p users", [UsersUploaded]),
      case (NumUploaders * NumberOfUsersPerUploader) =:= UsersUploaded of
        true -> {ok, State#state{users_uploaded=UsersUploaded}};
        false -> {error, upload_failed, State}
      end;
    {error, Reason} -> {error, Reason, State}
  end.

%% This has no effect on the tests, other than ensuring that we get some output
%% in the logs indicating how far along the data upload has progressed, or whether
%% it is still ongoing.
tally_uploaded_rows(NumSuccessful, NumFailed, NumPerMille) ->
  TotalSoFar = NumSuccessful + NumFailed,
  % If we have reached a new 0.X% of the upload, we output some state to the log,
  % otherwise we wait for more uploads to take place.
  case TotalSoFar rem NumPerMille == 0 of
    true ->
      Percentage = round(TotalSoFar / NumPerMille) / 10,
      ?INFO("Uploaded ~.1f%. ~p successfully and ~p failed", [Percentage, NumSuccessful, NumFailed]);
    false ->
      ok
  end,
  receive
    good -> tally_uploaded_rows(NumSuccessful+1, NumFailed, NumPerMille);
    bad -> tally_uploaded_rows(NumSuccessful, NumFailed+1, NumPerMille)
  after timer:minutes(10) ->
      ?INFO("Not received further tallies. Assuming data upload complete")
  end.

cloak_urls_and_headers(UploadState, #state{cloaks=Cloaks, analyst_id=AnalystId}) ->
  StateGen = case air_etcd:get("/settings/rails/global") of
    <<"true">> ->
      fun(Cloak) ->
        CloakUrl = "https://" ++ binary_to_list(Cloak) ++ "/bulk_insert",
        ?INFO("Using prod cloak at url ~p", [CloakUrl]),
        UploadState#upload_state{
          cloak_url = CloakUrl,
          headers = []
        }
      end;
    <<"false">> ->
      fun(Cloak) ->
        CloakUrl = "http://" ++ binary_to_list(Cloak) ++ ":8098/bulk_insert",
        ?INFO("Using dev cloak at url ~p", [CloakUrl]),
        UploadState#upload_state{
          cloak_url = CloakUrl,
          headers = [{"analyst", integer_to_list(AnalystId)}]
        }
      end
  end,
  [StateGen(Cloak) || Cloak <- Cloaks].

% Provides a barrier so the test doesn't proceed before
% the data has been uploaded, or we know that the data upload has failed.
wait_for_uploads_to_finish(0, Acc, _UploadPids) -> {ok, Acc};
wait_for_uploads_to_finish(N, Acc, UploadPids) ->
  receive
    % If one of the uplaoders fails, we want to abort the test outright.
    % This should not happen during normal operations!
    {error, upload_failed} ->
      abort_uploads(UploadPids),
      {error, upload_failed};
    {done, UserCount} -> wait_for_uploads_to_finish(N-1, Acc+UserCount, UploadPids);
    heartbeat -> wait_for_uploads_to_finish(N, Acc, UploadPids)
  after timer:minutes(10) ->
    ?ERROR("Data upload timed out"),
    abort_uploads(UploadPids),
    {error, timeout}
  end.

% We want to ability to stop uploads before they are complete.
% For example if one of the uploaders records that there is
% a failure, then letting the other uploaders continue is
% a waste of time.
abort_uploads(Pids) ->
  ?INFO("Upload of user data failed. Aborting"),
  [Pid ! abort || Pid <- Pids].

upload_data_for_user(0, UploadState, Acc, _TallyPid) -> UploadState#upload_state.return_pid ! {done, Acc};
upload_data_for_user(RemainingUsers, UploadState, Acc, TallyPid) ->
  % Our table has the following columns:
  % - int1 : bigint (64bit)
  % - int2 : bigint (64bit)
  % - double : double (64bit)
  % - text : text (480 bytes)
  % Each row weighs in at 504 bytes
  RowsForUser = rows_for_user(UploadState#upload_state.mean_rows_per_user),
  OneIfUpload = case RowsForUser > 0 of
    true ->
      UserNumStr = list_to_binary(integer_to_list(RemainingUsers) ++ pid_to_list(self())),
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
      upload_to_server(UploadState, list_to_binary(mochijson2:encode(Payload)), TallyPid),
      1;
    false -> 0
  end,
  % We allow aborting uploads in case a failure has happened in one
  % of the other uploaders.
  receive
    abort ->
      ?INFO("Uploading worker aborting"),
      aborted
  after 0 ->
    UploadState#upload_state.return_pid ! heartbeat,
    upload_data_for_user(RemainingUsers - 1, UploadState, Acc+OneIfUpload, TallyPid)
  end.

rows_for_user(MeanRowsPerUser) ->
  Rand1 = random_util:uniform(),
  Rand2 = random_util:uniform(),
  cloak_distributions:gauss(MeanRowsPerUser/4, MeanRowsPerUser, Rand1, Rand2).

upload_to_server(UploadState, Json, TallyPid) ->
  Request = {
    UploadState#upload_state.cloak_url,
    UploadState#upload_state.headers,
    "application/json",
    Json
  },
  HttpOptions = [
    {ssl, UploadState#upload_state.ssl_options},
    {timeout, timer:minutes(2)}
  ],
  case httpc:request(post, Request, HttpOptions, []) of
    {ok, {{_Version, 200, _StatusPhrase}, _Headers, Reply}} ->
      JsonResponse = mochijson2:decode(Reply),
      case ej:get({"success"}, JsonResponse) of
        true -> TallyPid ! good;
        _Other ->
          UploadState#upload_state.return_pid ! {error, upload_failed},
          ?WARNING("Received unexpected upload response: ~p", [Reply]),
          TallyPid ! bad
      end;
    Response ->
      UploadState#upload_state.return_pid ! {error, upload_failed},
      ?WARNING("Received unexpected upload response: ~p", [Response]),
      TallyPid ! bad
  end.

ssl_options({PrivateKeyPath, CertificatePath}) ->
  [
    {verify, verify_none},
    {keyfile, PrivateKeyPath},
    {certfile, CertificatePath}
  ].


%% -------------------------------------------------------------------
%% Run task
%% -------------------------------------------------------------------

-spec run_task(#state{}) -> step_response().
run_task(#state{cluster_id=ClusterId, table_name=TableName}=State) ->
  ?INFO("Running task against cluster"),
  TaskCode = ["
    -- We loop through all rows of data,
    -- to stress the streaming mechanism
    row_count = 0
    for row in user_table(\"" ++ TableName ++ "\") do
      row_count = row_count + 1
    end
    report_property(\"Users\", \"count\")
    -- While the max value really should be something like 10000,
    -- the cloak crashes when this is the case.
    quantize_props = {min = 0, max = 10, step=1}
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
      ?INFO("Task has been initiated. Waiting for results"),
      ChannelName = binary_to_list(ej:get({"channel_name"}, Response)),
      listen_for(ChannelName, State),
      ?INFO("Waiting for results on channel: ~p", [ChannelName]),
      %% We give the task 30 minutes to complete (30 * 60s)
      case wait_for(ChannelName, 1800, fun(R) -> {ok, R} end, State) of
        {ok, BinaryResultId} ->
          ResultId = binary_to_integer(BinaryResultId),
          ?INFO("Task finished. Result id: ~p", [ResultId]),
          validate_result(ResultId, State);
        {error, timeout} -> {error, timeout, State}
      end;
    Other -> Other
  end.

validate_result(ResultId, #state{users_uploaded=UserCount}=State) ->
  ?INFO("Validating results"),
  JSONSQLResult = air_db:call(fun(Connection) ->
        SQL = ["
          SELECT buckets_json
          FROM results
          WHERE results.id = $1"
        ],
        pgsql_connection:extended_query(SQL, [ResultId], Connection)
      end),
  case JSONSQLResult of
    {{select, 1}, [{JSONResult}]} ->
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
      ?INFO("Received noisy count of ~p. Expect it to lie within ~p and ~p. Real count: ~p",
          [NoisyUserCount, AllowedLowerBound, AllowedUpperBound, UserCount]),
      case (NoisyUserCount >= AllowedLowerBound) and (NoisyUserCount =< AllowedUpperBound) of
        true ->
          ?INFO("Result is ok"),
          {ok, State};
        false ->
          ?ERROR("Integration test failed because the returned count was invalid"),
          {error, unexpected_task_result, State}
      end;
    Other ->
      ?ERROR("Couldn't load task results from DB: ~p", [Other]),
      {error, no_task_result, State}
  end.


%% -------------------------------------------------------------------
%% Interacting with rails app
%% -------------------------------------------------------------------

rails_request(RequestPayload, State) ->
  JsonPayload = list_to_binary(mochijson2:encode(RequestPayload)),
  Url = binary_to_list(air_etcd:get("/service/infrastructure_api_local")) ++
      "/integration_tests",
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
      ?ERROR("Rails integration tests backend failed with reason: ~p", [Reason]),
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
        Other ->
          ?INFO("Received unexpected notification: ~p. Keeping on waiting", [Other]),
          next
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
      ?INFO("- ~p", [Message]),
      flush_messages()
  after 10 ->
      ok
  end.

-spec while_ok(#state{}, [atom()]) -> {ok | error, #state{}}.
while_ok(State, []) ->
  {ok, cleanup(State)};
while_ok(#state{timings=Timings}=State, [Function|Functions]) ->
  ?INFO("Performing step: ~p", [Function]),
  StartTimeSeconds = cloak_util:timestamp_to_epoch(now()),
  try ?MODULE:Function(State) of
    {ok, NewState} ->
      EndTimeSeconds = cloak_util:timestamp_to_epoch(now()),
      Duration = EndTimeSeconds - StartTimeSeconds,
      NewStateWithTiming = NewState#state{timings=[{Function, Duration}|Timings]},
      ?INFO("- ~p succeeded", [Function]),
      while_ok(NewStateWithTiming, Functions);
    {error, Reason, NewState} ->
      ?WARNING("Test failed in step ~p with reason: ~p", [Function, Reason]),
      {error, cleanup(NewState)}
  catch
    ProblemType:ProblemReason ->
      ?ERROR("Test failed with ~p:~p", [ProblemType, ProblemReason]),
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
