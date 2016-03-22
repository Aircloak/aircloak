%% @doc Queue workers.
%%      A queued worker is a system for processing jobs.  We only allow a fixed number of workers to be
%%      present per task.  If no worker is available the job is stored in an internal queue and scheduled as
%%      soon as we have a free worker.
%%
%%      In contrast to poolboy we do not monitor the processes that request a job to be processed.  This is
%%      done to cleanup the waiting queue in case that process dies.  The queued workers do not care about the
%%      requesting process and should be used when we do not wait for a reply and the requesting process might
%%      die on purpose before the job gets processed.
%%
%%      Tests for this module are implemented in {@link queued_worker_test} as property tests.
-module(queued_worker).
-behaviour(gen_server).

%% API
-export([
  start_link/3,
  run/2,
  stop/1
]).

%% gen_server callbacks
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
  handler :: atom(),
  queue = queue:new() :: queue:queue(),
  active_jobs = [] :: list(),
  max_number_of_jobs :: pos_integer(),
  number_of_active_jobs = 0 :: non_neg_integer()
}).

-callback run_job(Job) -> ok when Job :: any().


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Create a new queued_worker with given name, handler, and maximum number of worker processes.
-spec start_link(atom(), atom(), pos_integer()) -> {ok, pid()}.
start_link(Name, Handler, MaxJobs) ->
  gen_server:start_link({local, Name}, ?MODULE, [Handler, MaxJobs], []).

%% @doc Enqueue a new job in the queued_worker process.
-spec run(atom(), any()) -> ok.
run(Name, Job) ->
  gen_server:cast(Name, {run, Job}).

%% @doc Stop the specified queued_worker master.
-spec stop(atom()) -> ok.
stop(Name) ->
  gen_server:cast(Name, stop).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init([Handler, MaxJobs]) ->
  State = #state{
    handler = Handler,
    max_number_of_jobs = MaxJobs
  },
  {ok, State}.

handle_call(Request, From, State) ->
  larger:warning("unknown request ~p from ~p to ~p", [Request, From, ?MODULE]),
  {reply, unknown_request, State}.

handle_cast({run, Job}, #state{max_number_of_jobs=Max, number_of_active_jobs=Max, queue=Queue}=State) ->
  QueueWithNewJob = queue:in(Job, Queue),
  {noreply, State#state{queue=QueueWithNewJob}};
handle_cast({run, Job}, State) ->
  StateWithNewActiveJob = run_job(Job, State),
  {noreply, StateWithNewActiveJob};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason},
    #state{number_of_active_jobs=NumJobs, active_jobs=ActiveJobs}=State) ->
  case lists:member({Pid, MonitorRef}, ActiveJobs) of
    true ->
      ActiveJobsWithoutTerminated = [Job || Job <- ActiveJobs, {Pid, MonitorRef} /= Job],
      StateWithoutTerminated = State#state{
        number_of_active_jobs = NumJobs - 1,
        active_jobs = ActiveJobsWithoutTerminated
      },
      {noreply, conditionally_run_job(StateWithoutTerminated)};
    false ->
      ?WARNING("unknown monitored process ~p", [Pid]),
      {noreply, State}
  end.

terminate(normal, _State) ->
  ok;
terminate(Reason, _State) ->
  ?CRITICAL("abnormal termination of ~p due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, _) ->
  {error, not_implemented}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

run_job(Job, #state{handler=Handler, number_of_active_jobs=JobNum, active_jobs=ActiveJobs}=State) ->
  {Pid, MonitorRef} = spawn_monitor(fun() -> Handler:run_job(Job) end),
  State#state{number_of_active_jobs=JobNum+1, active_jobs=[{Pid, MonitorRef}|ActiveJobs]}.

conditionally_run_job(#state{queue=Queue}=State) ->
  case queue:out(Queue) of
    {empty, Queue} ->
      % No further job in queue, do nothing.
      State;
    {{value, Job}, QueueWithoutNextJob} ->
      % We have a pending job, just run the job.
      run_job(Job, State#state{queue=QueueWithoutNextJob})
  end.
