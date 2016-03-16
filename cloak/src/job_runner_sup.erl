%% @doc Supervisor for the {@link job_runner}.
%%      Please consult the job_runner documentation for further details.
-module(job_runner_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  execute/4,
  execute/5,
  execute/6,
  send_request_timeout/1
]).

%% Supervisor callback
-export([
  init/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec execute(#task{}, user_id(), job_data_streamer:job_input(), term(), job_runner:callback_fun()) -> ok.
execute(Req, User, JobInput, ReqId, SPid) ->
  execute(Req, User, JobInput, undefined, ReqId, SPid).

-spec execute(#task{}, user_id(), job_data_streamer:job_input(), job_accumulator(), term(), job_runner:callback_fun()) -> ok.
execute(Req, User, JobInput, Accumulator, ReqId, SPid) ->
  I = crypto:rand_uniform(0, runners_count()),
  job_runner:execute(I, Req, {User, JobInput}, Accumulator, ReqId, SPid).

-spec execute(#task{}, job_runner:get_parameters_fun(), term(), job_runner:callback_fun()) -> ok.
execute(Req, GetParametersFun, ReqId, SPid) when is_function(GetParametersFun, 1) ->
  lists:foreach(fun (I) ->
        ok = job_runner:execute(I, Req, GetParametersFun, undefined, ReqId, SPid)
      end, lists:seq(0, runners_count() - 1)).

%% Send a timeout of a given request to all runners.
-spec send_request_timeout(term()) -> ok.
send_request_timeout(ReqId) ->
  [job_runner:send_request_timeout(I, ReqId) || I <- lists:seq(0, runners_count() - 1)],
  ok.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  RunnersCount = case erlang:system_info(logical_processors_available) of
    unknown -> 2; % if we can't determine CPU count, assume 2
    Count -> Count
  end,
  application:set_env(cloak, runners_count, RunnersCount),
  ?INFO("Creating ~p job runners ...", [RunnersCount]),
  Processes = [
    {job_runner:name(I), {job_runner, start_link, [I]}, permanent, brutal_kill, worker, [job_runner]} ||
    I <- lists:seq(0, RunnersCount - 1)
  ],
  {ok, {{one_for_one, 1000000, 1}, Processes}}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-spec runners_count() -> pos_integer().
runners_count() ->
  {ok, RunnersCount} = application:get_env(cloak, runners_count),
  RunnersCount.
