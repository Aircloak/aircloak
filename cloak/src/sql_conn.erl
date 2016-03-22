%% @doc Services for opening and using sql connection
%%      This module serves as a thin wrapper around pgsql_connection. The main
%%      reason for doing this is to enforce timeouts. By default, pgsql_connection
%%      has no timeout, so we introduce this wrapper to enforce a timeout. If the
%%      timeout value is not provided, the default (specified in app config) will
%%      be used.
%%
%%      Note: timeout is applied on both sides (client and server). The raw timeout value
%%      is used on the server side. If the query times out, it will be canceled in the database
%%      and the function will return an error to its caller. In addition, the client-side timeout
%%      is set to 5 seconds + `server_timeout', so clients aren't blocked forever.
-module(sql_conn).

%% API
-export([
  start_link/1,
  simple_query/2,
  simple_query/3,
  simple_query_without_server_timeout/2,
  simple_query_without_server_timeout/3,
  extended_query/3,
  extended_query/4,
  batch_query/3,
  batch_query/4,
  sql_query/2,
  sql_query/3,
  foreach/3,
  foreach/4,
  foreach/5,
  fold/4,
  fold/5,
  fold/6,
  send_copy_data/2,
  send_copy_end/1
]).

-type connection() :: pid().

% Copy/paste from pgsql_connection, because they don't export the type
-type open_option() ::
        {host, inet:ip_address() | inet:hostname()} % default: ?DEFAULT_HOST
    |   {port, integer()}                       % default: ?DEFAULT_PORT
    |   {database, iodata()}                    % default: user
    |   {user, iodata()}                        % default: ?DEFAULT_USER
    |   {password, iodata()}                    % default: none
    |   {fetch_oid_map, boolean()}              % default: true
    |   {ssl, boolean()}                        % default: false
    |   {reconnect, boolean()}                  % default: true
    |   {application_name, atom() | iodata()}   % default: node()
    |   {timezone, iodata() | undefined}        % default: undefined (not set)
    |   {async, pid()}                          % subscribe to notifications (default: no)
    |   proplists:property().                   % undocumented !
-type open_options() :: [open_option()].
-type n_rows() :: integer().
-type row() :: tuple().
-type rows() :: [row()].
-type odbc_result_tuple() :: {updated, n_rows()} | {updated, n_rows(), rows()} | {selected, rows()}.

-export_type([
  connection/0
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the connection process.
-spec start_link(open_options()) -> {ok, pid()} | {error, any()}.
start_link(Options) ->
  pgsql_connection:start_link(Options).

%% @doc Perform a simple query without parameters.
-spec simple_query(iodata(), connection()) ->
  pgsql_connection:result_tuple() | {error, any()} | [pgsql_connection:result_tuple() | {error, any()}].
simple_query(Query, ConnPid) ->
    simple_query(Query, default_timeout(), ConnPid).

%% @doc Perform a simple query without parameters.
-spec simple_query(iodata(), pos_integer(), connection()) ->
  pgsql_connection:result_tuple() | {error, any()} | [pgsql_connection:result_tuple() | {error, any()}].
simple_query(Query, Timeout, ConnPid) ->
  pgsql_connection:simple_query(Query, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Perform a simple query with client-side timeout only.
%%      See {@link simple_query_without_server_timeout/3} for explanation.
-spec simple_query_without_server_timeout(iodata(), connection()) ->
    pgsql_connection:result_tuple() | {error, any()}.
simple_query_without_server_timeout(Sql, ConnPid) ->
  simple_query_without_server_timeout(Sql, default_timeout(), ConnPid).

%% @doc Perform a simple query with client-side timeout only.
%%      This function will not enforce a server-side timeout. Most of the time you
%%      should prefer {@link simple_query/3}. One situation where this function
%%      is needed is when invoking a COPY ... FROM STDIN. In such situation,
%%      enforcing a server-side timeout will cause a failure in the connection
%%      process. You can use this function instead, which runs a query with an infinite
%%      timeout from a separate process, and waits for the result for a finite amount
%%      of time.
-spec simple_query_without_server_timeout(iodata(), pos_integer(), connection()) ->
    pgsql_connection:result_tuple() | {error, any()}.
simple_query_without_server_timeout(Sql, Timeout, ConnPid) ->
  Me = self(),
  Ref = make_ref(),
  Pid = spawn_link(fun() -> Me ! {Ref, pgsql_connection:simple_query(Sql, pgsql_conn(ConnPid))} end),
  receive
    {Ref, Result} -> Result
  after Timeout ->
    % kill the separate process
    catch unlink(Pid),
    catch exit(Pid, kill),
    {error, timeout}
  end.

%% @doc Perform a query with parameters.
-spec extended_query(iodata(), [any()], connection()) ->
    pgsql_connection:result_tuple() | {error, any()}.
extended_query(Query, Parameters, ConnPid) ->
    extended_query(Query, Parameters, default_timeout(), ConnPid).

%% @doc Perform a query with parameters.
-spec extended_query(iodata(), [any()], pos_integer(), connection()) ->
    pgsql_connection:result_tuple() | {error, any()}.
extended_query(Query, Parameters, Timeout, ConnPid) ->
  pgsql_connection:extended_query(Query, Parameters, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Perform a query several times with parameters.
-spec batch_query(iodata(), [[any()]], connection()) ->
    [pgsql_connection:result_tuple()] | {error, any()} | [pgsql_connection:result_tuple() | {error, any()}].
batch_query(Query, Parameters, ConnPid) ->
    batch_query(Query, Parameters, default_timeout(), ConnPid).

%% @doc Perform a query several times with parameters.
-spec batch_query(iodata(), [[any()]], pos_integer(), connection()) ->
    [pgsql_connection:result_tuple()] | {error, any()} | [pgsql_connection:result_tuple() | {error, any()}].
batch_query(Query, Parameters, Timeout, ConnPid) ->
  pgsql_connection:batch_query(Query, Parameters, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Perform a query. See {@link sql_query/3} for details.
-spec sql_query(iodata(), connection()) -> odbc_result_tuple() | {error, any()}.
sql_query(Query, ConnPid) ->
  sql_query(Query, default_timeout(), ConnPid).

%% @doc Perform a query.
%%      This function creates a statement and runs step as many times as
%%      required. The result is:
%%      <ul>
%%      <li>``{selected, Rows}'' if the query was a SELECT query.</li>
%%      <li>``{updated, NbRows}'' if the query was not a SELECT query.</li>
%%      </ul>
%%      (the return types are compatible with ODBC's sql_query function).
-spec sql_query(iodata(), pos_integer(), connection()) -> odbc_result_tuple() | {error, any()}.
sql_query(Query, Timeout, ConnPid) ->
  pgsql_connection:sql_query(Query, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Iterate on results of a given query.
%%      The function is evaluated within the connection's process.
-spec foreach(fun((tuple()) -> any()), iodata(), connection()) -> ok | {error, any()}.
foreach(Function, Query, ConnPid) ->
  foreach(Function, Query, [], ConnPid).

%% @doc Iterate on results of a given query.
%%      The function is evaluated within the connection's process.
-spec foreach(fun((tuple()) -> any()), iodata(), [any()], connection()) -> ok | {error, any()}.
foreach(Function, Query, Parameters, ConnPid) ->
  foreach(Function, Query, Parameters, default_timeout(), ConnPid).

%% @doc Iterate on results of a given query.
%%      The function is evaluated within the connection's process.
-spec foreach(fun((tuple()) -> any()), iodata(), [any()], pos_integer(), connection()) -> ok | {error, any()}.
foreach(Function, Query, Parameters, Timeout, ConnPid) ->
  pgsql_connection:foreach(Function, Query, Parameters, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Fold over results of a given query.
%%      The function is evaluated within the connection's process.
-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), connection()) ->
      {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, ConnPid) ->
  fold(Function, Acc0, Query, [], ConnPid).

%% @doc Fold over results of a given query.
%%      The function is evaluated within the connection's process.
-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), [any()], connection()) ->
      {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Parameters, ConnPid) ->
  fold(Function, Acc0, Query, Parameters, default_timeout(), ConnPid).

%% @doc Fold over results of a given query.
%%      The function is evaluated within the connection's process.
-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), [any()], pos_integer(), connection()) ->
      {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Parameters, Timeout, ConnPid) ->
  pgsql_connection:fold(Function, Acc0, Query, Parameters, [], Timeout, pgsql_conn(ConnPid)).

%% @doc Send some binary data after starting a COPY
-spec send_copy_data(iodata(), connection()) -> ok | {error, any()}.
send_copy_data(Data, ConnPid) ->
  pgsql_connection:send_copy_data(Data, pgsql_conn(ConnPid)).

%% @doc Finish a COPY
-spec send_copy_end(connection()) -> {copy, integer()} | {error, any()}.
send_copy_end(ConnPid) ->
  pgsql_connection:send_copy_end(pgsql_conn(ConnPid)).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

pgsql_conn(ConnPid) ->
  {pgsql_connection, ConnPid}.

default_timeout() ->
  cloak_conf:get_val(cloak_db, default_query_timeout).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").
-include("debug_helpers.hrl").

-define(assertQueryTimeout(QueryResult),
      (fun() ->
        ?assertMatch({error, {pgsql_error, _}}, QueryResult),
        {error, {pgsql_error, Fields}} = QueryResult,
        ?assertEqual(<<"canceling statement due to statement timeout">>, proplists:get_value(message, Fields))
      end)()
    ).

-define(_assertQueryTimeout(Query), fun() -> ?assertQueryTimeout(Query) end).

query_test_() ->
  {foreach,
    fun() ->
      {ok, ConnPid} = start_link(db_test:conn_params()),
      ConnPid
    end,
    fun(ConnPid) ->
      unlink(ConnPid),
      exit(ConnPid, kill)
    end,
    [
      fun(ConnPid) -> ?_assertEqual({{select, 1}, [{1}]}, simple_query("SELECT 1", ConnPid)) end,
      fun(ConnPid) -> ?_assertQueryTimeout(simple_query("SELECT pg_sleep(1)", 10, ConnPid)) end,
      fun(ConnPid) ->
        ?_assertEqual({{select, 1}, [{1}]}, simple_query_without_server_timeout("SELECT 1", ConnPid))
      end,
      fun(ConnPid) ->
        ?_assertEqual({error, timeout}, simple_query_without_server_timeout("SELECT pg_sleep(1)", 10, ConnPid))
      end,
      fun(ConnPid) -> ?_assertEqual({{select, 1}, [{2}]}, extended_query("SELECT $1 + 1", [1], ConnPid)) end,
      fun(ConnPid) -> ?_assertQueryTimeout(extended_query("SELECT pg_sleep($1)", [1], 10, ConnPid)) end,
      fun(ConnPid) ->
        ?_assertEqual(
          [{{select, 1}, [{2}]}, {{select, 1}, [{3}]}, {{select, 1}, [{4}]}],
          batch_query("SELECT $1 + 1", [[1], [2], [3]], ConnPid)
        )
      end,
      fun(ConnPid) ->
        fun() ->
          [R1, R2, R3] = batch_query("SELECT pg_sleep($1)", [[2], [0], [1]], 100, ConnPid),
          ?assertQueryTimeout(R1),
          ?assertEqual({{select, 1}, [{null}]}, R2),
          ?assertQueryTimeout(R3)
        end
      end,
      fun(ConnPid) -> ?_assertEqual({selected, [{1}]}, sql_query("SELECT 1", ConnPid)) end,
      fun(ConnPid) -> ?_assertQueryTimeout(sql_query("SELECT pg_sleep(1)", 10, ConnPid)) end,
      fun(ConnPid) -> ?_assertEqual(ok, foreach(fun({1}) -> ok end, "SELECT 1", ConnPid)) end,
      fun(ConnPid) -> ?_assertEqual(ok, foreach(fun({2}) -> ok end, "SELECT 1 + $1", [1], ConnPid)) end,
      fun(ConnPid) ->
        ?_assertQueryTimeout(foreach(fun(_) -> ?assert(false) end, "SELECT pg_sleep(1)", [], 10, ConnPid))
      end,
      fun(ConnPid) -> ?_assertEqual({ok, 3}, fold(fun({Y}, X) -> X+Y end, 1, "SELECT 2", ConnPid)) end,
      fun(ConnPid) -> ?_assertEqual({ok, 4}, fold(fun({Y}, X) -> X+Y end, 1, "SELECT $1 + 2", [1], ConnPid)) end,
      fun(ConnPid) -> ?_assertQueryTimeout(fold(fun({Y}, X) -> X+Y end, 1, "SELECT pg_sleep(1)", [], 10, ConnPid)) end
    ]
  }.

-endif.
