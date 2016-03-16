%% @doc Supervisor for service used for sending
%%      anonymized results back to the air from
%%      the cloak
-module(result_sender_sup).
-behaviour(supervisor).

%% supervisor callbacks
-export([
  start_link/0,
  init/1
]).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @private
init ([]) ->
  {ok, {{simple_one_for_one, 10, 10}, [
    {undefined, {result_sender, start_link, []},
    transient, 5000, worker, [result_sender]}
  ]}}.
