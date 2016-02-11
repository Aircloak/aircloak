%% @doc Used to plug into the insertion workflow of `job_runner'.
-module(job_insert_mock).

%% Internal API
-export([
  validate/2,
  queue_inserts/3
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% Internal API functions
%% -------------------------------------------------------------------

%% @hidden
validate(_Analyst, _UserData) ->
  {[], []}.

%% @hidden
queue_inserts(Analyst, User, UserData) ->
  case gproc:where({n, l, {task_executor, Analyst}}) of
    undefined -> ok;
    Pid ->
      Pid ! {insert, Analyst, User, UserData}
  end,
  ok.
