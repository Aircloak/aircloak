%% Various test helpers


%% -------------------------------------------------------------------
%% Chainable test suites
%% -------------------------------------------------------------------


%% Can be used to easily combine multiple setup/teardown functions. Some helpers
%% are provided out of the box, with macros in this file.
%%
%% Example:
%%
%%   ?test_suite(my_test_,
%%         setup,
%%         % Before tests starts required applications and processes
%%         % After tests, terminates all processes and stops all started applications
%%         [
%%           ?with_applications([gproc, pgsql]),
%%           ?with_processes([cloak_db_pool_sup])
%%         ],
%%         [
%%           % place standard tests here
%%         ]
%%       ).
-define(test_suite(Name, Type, Specs, Tests),
      Name() ->
        {Setups, Teardowns} = lists:unzip(lists:flatten(Specs)),
        {
          Type,
          fun() ->
            [SetupFun() || SetupFun <- Setups]
          end,
          fun(SetupResults) ->
            [Fun(Arg) || {Fun, Arg} <- lists:reverse(lists:zip(Teardowns, SetupResults))]
          end,
          Tests
        }
      ).

-define(with_processes(ProcessesSpec),
  {
    fun() ->
      [
        begin
          {ok, Pid} = case ProcessSpec of
            {M, F, A} -> apply(M, F, A);
            {M, F} -> apply(M, F, []);
            M when is_atom(M) -> apply(M, start_link, [])
          end,
          Pid
        end || ProcessSpec <- ProcessesSpec]
    end,
    fun(Pids) ->
      error_logger:tty(false),
      [
        begin
          unlink(Pid),
          exit(Pid, kill)
        end || Pid <- lists:reverse(Pids)
      ],
      timer:sleep(200),
      error_logger:tty(true)
    end
  }).

-define(with_applications(Applications),
  {
    fun() ->
      error_logger:tty(false),
      Started = lists:flatten([
            begin
              {ok, StartedApps} = application:ensure_all_started(Application),
              StartedApps
            end || Application <- Applications
          ]),
      error_logger:tty(true),
      Started
    end,
    fun(StartedApps) ->
      error_logger:tty(false),
      [application:stop(App) || App <- StartedApps],
      timer:sleep(200),
      error_logger:tty(true)
    end
  }).

-define(load_conf, [{fun() -> air_conf:load_test_config() end, fun(_) -> ok end}]).

-define(with_db, [?with_applications([gproc, pgsql, etcd]), ?with_processes([air_etcd, cloak_db_pool_sup])]).

-define(sanbox_web_server, [{fun() -> air_sandbox_web:setup_routes() end, fun(_) -> ok end}]).


%% -------------------------------------------------------------------
%% Database query helpers
%% -------------------------------------------------------------------

-define(db_simple_query(Query),
      air_db:call(fun(Connection) -> pgsql_connection:simple_query(Query, Connection) end)
    ).

-define(db_batch_query(Query, Params),
      air_db:call(fun(Connection) -> pgsql_connection:batch_query(Query, Params, Connection) end)
    ).

-define(db_insert_rows(Table, Fields, Rows), (
      fun() ->
        {Params, _} = lists:foldl(
              fun(_, {Acc, Index}) -> {[[$$, Index + $0] | Acc], Index + 1} end,
              {[], 1},
              Fields
            ),
        Query = io_lib:format("INSERT INTO ~s (~s) VALUES(~s)",
            [Table, string:join(Fields, ","), string:join(lists:reverse(Params), ",")]),
        ?db_batch_query(Query, Rows)
      end
    )()).
