-module(gen_etcd_watcher_test).
-behaviour(gen_etcd_watcher).


%% -------------------------------------------------------------------
%% Test gen_server
%% -------------------------------------------------------------------

-export([
  init/1,
  handle_key_change/2,
  handle_info/2,
  terminate/2
]).

start_link(Key) ->
  gen_etcd_watcher:start_link(?MODULE, Key, self()).

init(Pid) -> {ok, Pid}.

handle_key_change(ChangeInfo, Pid) ->
  Pid ! {changed, ChangeInfo},
  {noreply, Pid}.

handle_info(Message, Pid) ->
  Pid ! {info, Message},
  {noreply, Pid}.

terminate(_, _) -> ok.


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
          Key = etcd_test_helper:unique_key(),
          {ok, Pid} = start_link(Key),
          air_etcd:set(Key, "value1"),
          ?assertReceived({changed, Key}, 500),
          air_etcd:delete(Key),
          ?assertReceived({changed, Key}, 500),
          air_etcd:set(Key, "value2", 1),
          ?assertReceived({changed, Key}, 500),
          ?assertReceived({changed, Key}, 3000),
          unlink(Pid),
          exit(Pid, normal)
        end}
      ]
    ).
