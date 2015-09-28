-module(gen_etcd_leader_test).
-behaviour(gen_etcd_leader).

-include("air.hrl").

%% -------------------------------------------------------------------
%% Test callback implementation
%% -------------------------------------------------------------------

-export([
  init/1,
  handle_leader/1,
  handle_follower/1,
  handle_info/2,
  terminate/2
]).

start_link(Id) ->
  gen_etcd_leader:start_link(?MODULE, "test_leader", {self(), Id}).

init({Pid, Id}) -> {ok, {Pid, Id}}.

handle_leader({Pid, Id} = State) ->
  Pid ! {leader, Id},
  {ok, State}.

handle_follower({Pid, Id} = State) ->
  Pid ! {follower, Id},
  {ok, State}.

handle_info(Message, {Pid, Id} = State) ->
  Pid ! {message, Id, Message},
  {noreply, State}.

terminate(_, {Pid, Id}) ->
  Pid ! {terminate, Id}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").
-include_lib("erlang_common/include/eunit_helpers.hrl").

?test_suite(standard_test_,
      setup,
      [
        ?with_applications([hackney, etcd])
      ],
      [
        {"test", fun() ->
          {ok, Pid1} = start_link(1),
          ?assertReceived({leader, 1}, 500),
          {ok, Pid2} = start_link(2),
          ?assertReceived({follower, 2}, 500),
          unlink(Pid1),
          exit(Pid1, shutdown),
          ?assertReceived({terminate, 1}, 500),
          ?assertReceived({leader, 2}, 500),
          Pid2 ! some_message,
          ?assertReceived({message, 2, some_message}, 500)
        end}
      ]
    ).
