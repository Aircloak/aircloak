-module(airpub_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUP(Name), ?SUP(Name, Name, [])).
-define(SUP(SupId, SupModule, StartArgs),
    {SupId, {SupModule, start_link, StartArgs}, permanent, infinity, supervisor, [SupModule]}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    ?CHILD(history, worker),
    ?SUP(js_vm_sup),
    % This worker must be started at the end, when all other airpub
    % processes are initialized.
    ?CHILD(airpub_leader, worker)
  ]}}.
