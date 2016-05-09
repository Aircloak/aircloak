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
-define(test_suite(Name, Type, Specs, Tests), Name() ->
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
    fun() -> [
        begin
          {ok, Pid} = case ProcessSpec of
            {M, F, A} -> apply(M, F, A);
            {M, F} -> apply(M, F, []);
            M when is_atom(M) -> apply(M, start_link, [])
          end,
          Pid
        end || ProcessSpec <- ProcessesSpec
      ]
    end,
    fun(Pids) ->
      error_logger:tty(false), [
        begin
          unlink(Pid),
          monitor(process, Pid),
          exit(Pid, shutdown),
          receive
            {'DOWN', _MRef, process, Pid, _} -> ok
          after 1000 ->
            exit(Pid, kill)
          end
        end || Pid <- lists:reverse(Pids)
      ],
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
      % It seems that when application:stop returns, some processes might still
      % linger for awhile. Therefore, we introduce a magical sleep here.
      timer:sleep(200),
      error_logger:tty(true)
    end
  }).

-define(load_conf, [{fun() -> air_conf:load_test_config() end, fun(_) -> ok end}]).

-define(with_db, [?with_applications([gproc, pgsql, etcd]), ?with_processes([cloak_db_pool_sup])]).

-define(api_web_server, [{fun() -> air_api_sup:setup_routes() end, fun(_) -> ok end}]).

