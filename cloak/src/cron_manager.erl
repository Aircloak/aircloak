%% @doc This module acts as a thin wrapper around `erlcron', making sure that
%%      no multiple instances of any job are running.
-module(cron_manager).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  install/1
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("cloak.hrl").

-record(state, {
  running_jobs = sets:new() :: set:set(),
  pid_job_map = dict:new() :: dict:dict()
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%% @doc Installs cron jobs
-spec install([erlcron:job()] | erlcron:job()) -> ok.
install(Jobs) when is_list(Jobs) ->
  [install(Job) || Job <- Jobs];
install({RunWhen, Callable}) ->
  erlcron:cron({RunWhen, fun(JobRef, At) -> gen_server:cast(?MODULE, {invoke, JobRef, At, Callable}) end}),
  ok.


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
handle_cast({invoke, JobRef, At, Callable}, State) ->
  case sets:is_element(JobRef, State#state.running_jobs) of
    true ->
      ?WARNING("Job ~p is still running, skipping execution.", [Callable]),
      {noreply, State};
    false ->
      Pid = spawn_link(fun() -> invoke(Callable, JobRef, At) end),
      {noreply, State#state{
        running_jobs = sets:add_element(JobRef, State#state.running_jobs),
        pid_job_map = dict:store(Pid, JobRef, State#state.pid_job_map)
      }}
  end;
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({'EXIT', Pid, _}, State) ->
  case dict:find(Pid, State#state.pid_job_map) of
    error -> {noreply, State};
    {ok, JobRef} ->
      {noreply, State#state{
        running_jobs = sets:del_element(JobRef, State#state.running_jobs),
        pid_job_map = dict:erase(Pid, State#state.pid_job_map)
      }}
  end;
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

invoke({M, F, A}, _, _) -> erlang:apply(M, F, A);
invoke(Fun, JobRef, At) when is_function(Fun) -> Fun(JobRef, At).
