%% @doc Distributes execution of the given task across all nodes in the cluster. Collects
%%      responses and forwards to the task cache.
-module(task_coordinator).
-behaviour(queued_worker).
-behaviour(gen_server).

%% API
-export([
  run_task/1,
  block_run_task/1
]).

%% queued_worker callbacks
-export([
  run_job/1
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
-include("sandbox_pb.hrl").

-record(state, {
  task :: #task{},
  start_time = os:timestamp() :: erlang:timestamp(),
  aggregator :: aggregator:aggregator(),
  lcf_users :: lcf_users:lcf_users(),
  job_parameters = [] :: [job_runner:parameters()],
  active_jobs = 0 :: non_neg_integer(),
  reply_pid :: pid() | undefined,
  reply_ref :: reference() | undefined
}).

-define(RUN_ON_MASTER_MAX_RETRIES, 16).  %% how many times we try to push the job to the master node
-define(RUN_ON_MASTER_WAIT, 100).  %% how many ms we wait between retries


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Asynchronously runs the given task on all nodes in the cluster.
-spec run_task(#task{}) -> ok.
run_task(Task) ->
  queued_worker:run(?MODULE, {Task, undefined, undefined}).

%% @doc Runs the task, and returns after the task is executed, and results reported back.
%%      Note: the result is always `ok', regardless of whether the task completed successfully, or some error
%%      occured.
-spec block_run_task(#task{}) -> ok | {error, timeout}.
block_run_task(Task) ->
  CallerRef = make_ref(),
  queued_worker:run(?MODULE, {Task, self(), CallerRef}),
  receive
    {finished, CallerRef} -> ok
  after cloak_conf:get_val(queries, sync_query_timeout) ->
    {error, timeout}
  end.


%% -------------------------------------------------------------------
%% queued_worker callbacks
%% -------------------------------------------------------------------

%% @hidden
run_job({Task, CallerPid, CallerRef}) ->
  TaskRef = make_ref(),
  gen_server:start_link(?MODULE, {Task, self(), TaskRef}, []),
  receive
    {finished, TaskRef} -> notify_caller(CallerPid, CallerRef)
  after cloak_conf:get_val(queries, sync_query_timeout) ->
    notify_caller(CallerPid, CallerRef)
  end,
  ok.


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init({Task, ReplyPid, ReplyRef}) ->
  init({Task, ReplyPid, ReplyRef, fun job_runner_sup:execute/4});
init({Task, ReplyPid, ReplyRef, JobRunner}) ->
  % Notify possible subscribers that this process has started. Used for test purposes.
  gproc:send({p, l, {task_listener, Task#task.task_id}}, {task, Task#task.task_id, {started, self()}}),
  LcfUsers = lcf_users:new(),
  Aggregator = aggregator:new(LcfUsers),
  State = #state{
    aggregator = Aggregator,
    lcf_users = LcfUsers,
    start_time = os:timestamp(),
    task = Task,
    job_parameters = ?MEASURE("task.prefetch",
        job_data_streamer:create_job_inputs(Task#task.prefetch)),
    reply_pid = ReplyPid,
    reply_ref = ReplyRef
  },
  progress_handler:add_unfinished_jobs(Task, length(State#state.job_parameters) + 1),
  case State#state.job_parameters of
    [] ->
      % No input data, so we immediately respond with a success and stop
      on_task_finished(success, State),
      ignore;
    [_|_] ->
      % Some input data -> forward to job runners
      ok = JobRunner(
              Task,
              fun (This) ->
                gen_server:call(This, get_parameters, infinity) % invoke coordinating server to get the parameters for next job
              end,
              self(),
              fun (This, {_JobReturnCode, JobResponse}) ->
                try aggregator:add_job_response(JobResponse, Aggregator) catch _:_ -> ok end, % store response
                gen_server:cast(This, job_finished) % inform coordinating server that job finished
              end
            ),
      % create timer to cancel task on timeout
      erlang:send_after(cloak_conf:get_val(queries, query_timeout), self(), timeout),
      {ok, State}
  end.

%% @hidden
handle_call(get_parameters, _From, State) ->
  {Parameters, NewState} = get_next_job_parameters(State),
  {reply, Parameters, NewState};
handle_call(_Message, From, State) ->
  ?ERROR("unknown call to task_local_runner from ~p", [From]),
  {noreply, State}.

%% @hidden
handle_cast(job_finished, State = #state{job_parameters = [], active_jobs = 1}) ->
  % we finished executing the task, report and stop
  on_task_finished(success, State),
  {stop, normal, State};
handle_cast(job_finished, State = #state{active_jobs = ActiveJobs}) when ActiveJobs > 0 ->
  % we have more work to do, keep waiting
  try progress_handler:job_of_task_finished(State#state.task) catch _:_ -> ok end,
  {noreply, State#state{active_jobs = ActiveJobs - 1}}.

%% @hidden
handle_info(timeout, State) ->
  % processing timeout reached, cancel jobs and report what we have
  ?WARNING("task runner timeout"),
  on_task_finished(timeout, State),
  {stop, normal, State}.

% @hidden
terminate(normal, _) ->
  ok;
terminate(Error, State) ->
  case cloak_conf:in_development() of
    false ->
      %% Do not give out the Error as it might leak sensitive data.
      ?ERROR("task_coordinator failure");
    true ->
      ?ERROR("task_coordinator failure: ~p", [Error])
  end,
  cloak_metrics:count("task.failure"),
  progress_handler:unregister_task(State#state.task),
  Results = [#bucket_report{
    label = #bucket_label{label = ?JOB_EXECUTION_ERROR, value = <<"task coordinator crashed">>},
    count = 1,
    noisy_count = 1,
    users_hash = <<"">>,
    noise_sd = 1
  }],
  ?MEASURE("task.send_result", begin
        #task{task_id = TaskId, result_destination = ResultDestination} = State#state.task,
        result_sender:send_results(TaskId, ResultDestination, Results)
      end),
  ?REPORT_DURATION("task.total", State#state.start_time).

code_change(_, _, _) ->
  {error, not_implemented}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

notify_caller(undefined, _) -> ok;
notify_caller(CallerPid, CallerRef) ->
  CallerPid ! {finished, CallerRef}.

on_task_finished(FinishReason, State) ->
  try progress_handler:job_of_task_finished(State#state.task) catch _:_ -> ok end,
  progress_handler:unregister_task(State#state.task),
  case FinishReason of
    success -> cloak_metrics:count("task.successful");
    timeout ->
      cloak_metrics:count("task.timeout"),
      job_runner_sup:send_request_timeout(self())
  end,
  process_responses(FinishReason, State).

% This is called by the job_runners when they are ready to execute the next job from the current task.
% It will return the next job parameters if there's any.
-spec get_next_job_parameters(#state{}) -> {undefined | job_runner:parameters(), #state{}}.
get_next_job_parameters(State = #state{job_parameters = []}) ->
  {undefined, State};
get_next_job_parameters(State = #state{active_jobs = ActiveJobs,
      job_parameters = [NextJobParameters | RemainingJobParameters]}) ->
  {NextJobParameters, State#state{job_parameters = RemainingJobParameters, active_jobs = ActiveJobs + 1}}.

process_responses(FinishReason, #state{task=#task{type=batch}} = State) ->
  Reports = aggregator:reports(State#state.aggregator),
  Results = ?MEASURE("task.anonymization", anonymizer:anonymize(Reports, State#state.lcf_users)),
  ?MEASURE("task.send_result", begin
        #task{task_id = TaskId, result_destination = ResultDestination} = State#state.task,
        FinalResults = Results ++ report_finish_reason(FinishReason),
        result_sender:send_results(TaskId, ResultDestination, FinalResults)
      end),
  case State#state.reply_pid of
    undefined -> ok;
    Pid -> Pid ! {finished, State#state.reply_ref}
  end,
  ?REPORT_DURATION("task.total", State#state.start_time).

-spec report_finish_reason(success | timeout) -> [#bucket_report{}].
report_finish_reason(success) ->
  []; % nothing to report
report_finish_reason(timeout) ->
  [#bucket_report{
    label = #bucket_label{label = ?JOB_EXECUTION_ERROR,
    value = <<"task execution timed out before processing all users">>},
    count = 1, noisy_count = 1, users_hash = <<"">>, noise_sd = 1
  }].
