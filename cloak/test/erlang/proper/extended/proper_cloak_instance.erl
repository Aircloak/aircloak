%% This module contains code to start and handle cloak-core instances for PropEr.
-module(proper_cloak_instance).

-include("cloak.hrl").

%% API
-export([
  start_instance/0,
  start_instance/1,
  start_instance/2,
  stop_instance/0,
  stop_instance/1,
  default_cloak_node/0,
  cloak_node/1,
  cloak_http_port/1,
  cloak_listen_port/1,
  cloak_dbname/1
]).

%% Internal API
-export([
  start_cloak_application/0
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_instance() ->
  start_instance(1).

start_instance(NodeNum) ->
  start_instance(NodeNum, true).

start_instance(NodeNum, ClearDb) ->
  ensure_service_started(NodeNum),
  Handler = handler_name(NodeNum),
  Handler ! {start, self(), ClearDb},
  receive
    {Handler, start, Node} ->
      {ok, Node};
    {'EXIT', Reason} ->
      throw({'cannot start cloak', Reason})
  after 60000 ->
    throw({timeout, Handler})
  end.

stop_instance() ->
  stop_instance(1).

stop_instance(NodeNum) ->
  ensure_service_started(NodeNum),
  Handler = handler_name(NodeNum),
  Handler ! {stop, self()},
  receive
    {Handler, stop} ->
      ok;
    {'EXIT', Reason} ->
      throw({'cannot stop cloak', Reason})
  after 60000 ->
    throw({timeout, Handler})
  end.

default_cloak_node() -> cloak_node(1).

cloak_node(I) -> list_to_atom("cloak" ++ integer_to_list(I) ++ "@localhost").

cloak_http_port(I) -> 8089+I.

cloak_listen_port(I) -> 34423+I.

cloak_dbname(I) -> "cloaktest" ++ integer_to_list(I).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

handler_name(NodeNum) ->
  list_to_atom(atom_to_list(?MODULE) ++ "--" ++ atom_to_list(cloak_node(NodeNum))).

ensure_service_started(NodeNum) ->
  Name = handler_name(NodeNum),
  case whereis(Name) of
    undefined ->
      Pid = spawn_link(fun() ->
        port_master(Name, NodeNum, os:getenv("PROPER_NODE_STDOUT") =:= "true", not_running)
      end),
      register(Name, Pid);
    _ ->
      ok
  end.

port_master(Handler, NodeNum, PrintNodeStdout, not_running) ->
  receive
    {start, Client, ClearDb} ->
      Node = cloak_node(NodeNum),
      Args = [
        atom_to_list(Node),
        integer_to_list(cloak_http_port(NodeNum)),
        integer_to_list(cloak_listen_port(NodeNum)),
        cloak_dbname(NodeNum),
        atom_to_list(ClearDb)
      ],
      Port = open_port({spawn_executable, "./start-proper-instance"},
          [{line, 1024}, use_stdio, exit_status, {args, Args}]),
      wait_for_instance(Node),
      start_cloak_application_on_instance(Node),
      Client ! {Handler, start, Node},
      port_master(Handler, NodeNum, PrintNodeStdout, {running, Port});
    {stop, Client} ->
      Client ! {Handler, stop},
      port_master(Handler, NodeNum, PrintNodeStdout, not_running);
    Message ->
      print_message(PrintNodeStdout, NodeNum, Message),
      port_master(Handler, NodeNum, PrintNodeStdout, not_running)
  end;
port_master(Handler, NodeNum, PrintNodeStdout, {running, Port}) ->
  receive
    {stop, Client} ->
      Node = cloak_node(NodeNum),
      rpc:call(Node, init, stop, []),
      port_master(Handler, NodeNum, PrintNodeStdout, {shutdown, Port, Client});
    Message ->
      print_message(PrintNodeStdout, NodeNum, Message),
      port_master(Handler, NodeNum, PrintNodeStdout, {running, Port})
  end;
port_master(Handler, NodeNum, PrintNodeStdout, {shutdown, Port, Client}) ->
  receive
    {Port, {exit_status, _Status}} ->
      Client ! {Handler, stop},
      port_master(Handler, NodeNum, PrintNodeStdout, not_running);
    {'EXIT', Port, _Reason} ->
      Client ! {Handler, stop},
      port_master(Handler, NodeNum, PrintNodeStdout, not_running);
    Message ->
      print_message(PrintNodeStdout, NodeNum, Message),
      port_master(Handler, NodeNum, PrintNodeStdout, {shutdown, Port, Client})
  end.

print_message(false, _, Message) -> ok;
print_message(true, NodeNum, {_, {data, {eol, String}}}) ->
  io:format("~p: ~s~n", [cloak_node(NodeNum), String]);
print_message(true, NodeNum, {_, {data, {noeol, String}}}) ->
  io:format("~p: ~s~n", [cloak_node(NodeNum), String]);
print_message(true, NodeNum, OtherData) ->
  io:format("~n~p: ~p~n", [cloak_node(NodeNum), OtherData]).

wait_for_instance(Node) ->
  wait_for_instance(Node, 100).

wait_for_instance(Node, 0) ->
  throw({'cannot connect to node', Node});
wait_for_instance(Node, N) ->
  case net_adm:ping(Node) of
    pong ->
      ok;
    pang ->
      timer:sleep(300),
      wait_for_instance(Node, N-1)
  end.

start_cloak_application_on_instance(Node) ->
  start_cloak_application_on_instance(Node, 100).

start_cloak_application_on_instance(Node, 0) ->
  throw({'cannot start cloak application on instance', Node});
start_cloak_application_on_instance(Node, N) ->
  case rpc:call(Node, ?MODULE, start_cloak_application, []) of
    {ok, _} ->
      wait_for_cloak_core(Node, 100);
    {error, _} ->
      timer:sleep(300),
      start_cloak_application_on_instance(Node, N - 1);
    {badrpc, nodedown} ->
      timer:sleep(300),
      start_cloak_application_on_instance(Node, N - 1);
    Message ->
      throw({'illegal message', Message})
  end.

start_cloak_application() ->
  application:ensure_all_started(cloak).

wait_for_cloak_core(Node, 0) ->
  throw({'cloak node does not get ready', Node});
wait_for_cloak_core(Node, N) ->
  case catch rpc:call(Node, node_state, is_ready, []) of
    true ->
      ok;
    _Otherwise ->
      timer:sleep(300),
      wait_for_cloak_core(Node, N-1)
  end.
