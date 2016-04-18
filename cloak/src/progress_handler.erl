%% @doc This module implements a progress counter for tasks.
%%      It's purpose is to give analysts a way to see if the execution progresses and a hint how long it will
%%      take to finish processing the task.
-module(progress_handler).
-behaviour(gen_server).

-include("cloak.hrl").

%% API
-export([
  start_link/0,
  register_task/1,
  unregister_task/1,
  add_unfinished_jobs/2,
  job_of_task_finished/1
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

-record(task_progress, {
  task :: #task{},
  number_of_jobs = 0 :: non_neg_integer(),
  finished_jobs = 0 :: non_neg_integer(),
  progress = 0 :: non_neg_integer(), % this is the anonymized progress that should be reported
  last_reported_progress = 0 :: non_neg_integer(),
  last_report_timestamp = erlang:monotonic_time() :: non_neg_integer()
}).

-type state() :: #{binary() => #task_progress{}}.


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a task to be progress-supervised.
%%      In addition to registering this task in the progress supervisor it will also add the unique ID to the
%%      task record.
-spec register_task(#task{}) -> #task{}.
register_task(Task) ->
  gen_server:call(?MODULE, {register_task, Task}).

%% @doc Unregister a progress-supervised task.
-spec unregister_task(#task{}) -> ok.
unregister_task(#task{progress_handle = undefined}) ->
  ok; % No progress handler registered: just ignore.
unregister_task(#task{progress_handle = Handle}) ->
  gen_server:cast(?MODULE, {unregister_task, Handle}).

%% @doc Set the number of jobs for the given task.
-spec add_unfinished_jobs(#task{}, non_neg_integer()) -> ok | {error, any()}.
add_unfinished_jobs(#task{progress_handle = undefined}, _NumOfJobs) ->
  ok; % No progress handler registered: just ignore.
add_unfinished_jobs(#task{progress_handle = Handle}, NumOfJobs) ->
  gen_server:cast(?MODULE, {add_unfinished_jobs, Handle, NumOfJobs}).

%% @doc Register that a job of a progress-supervised task finished.
-spec job_of_task_finished(#task{}) -> ok.
job_of_task_finished(#task{progress_handle = undefined}) ->
  ok; % No progress handler registered: just ignore.
job_of_task_finished(#task{progress_handle = Handle}) ->
  gen_server:cast(?MODULE, {job_of_task_finished, Handle}).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

-spec init(any()) -> {ok, state()}.
init(_) ->
  {ok, #{}}.

handle_call({register_task, Task}, _From, RegisteredTasks) ->
  {NewTask, NewRegisteredTasks} = register_task(Task, RegisteredTasks),
  {reply, NewTask, NewRegisteredTasks}.

handle_cast({add_unfinished_jobs, Handle, NumOfJobs}, RegisteredTasks) ->
  case maps:find(Handle, RegisteredTasks) of
    error ->
      ?ERROR("add_unfinished_jobs called for unknown handle ~p", [Handle]),
      {noreply, RegisteredTasks};
    {ok, #task_progress{number_of_jobs = OldNumOfJobs} = TaskProgress} ->
      NewTaskProgress = TaskProgress#task_progress{
        number_of_jobs = OldNumOfJobs + NumOfJobs,
        progress = 0  % reset the progress in case we had already finished processing of jobs on another node
      },
      NewTaskProgressWithUpdatedReport = update_progress_report(NewTaskProgress),
      {noreply, RegisteredTasks#{Handle := NewTaskProgressWithUpdatedReport}}
  end;
handle_cast({unregister_task, Handle}, RegisteredTasks) ->
  case maps:is_key(Handle, RegisteredTasks) of
    true ->
      {noreply, maps:remove(Handle, RegisteredTasks)};
    false ->
      ?WARNING("Cannot remove unknown handle from progress manager"),
      {noreply, RegisteredTasks}
  end;
handle_cast({job_of_task_finished, Handle}, RegisteredTasks) ->
  case maps:find(Handle, RegisteredTasks) of
    error ->
      ?WARNING("reported progress for a task unknown to the supervisor"),
      {noreply, RegisteredTasks};
    {ok, #task_progress{finished_jobs = Jobs} = TaskProgress} ->
      NewTaskProgress = TaskProgress#task_progress{finished_jobs = Jobs+1},
      NewTaskProgressWithUpdatedReport = update_progress_report(NewTaskProgress),
      {noreply, RegisteredTasks#{Handle := NewTaskProgressWithUpdatedReport}}
  end.

handle_info(_Message, State) ->
  % not implemented
  {noreply, State}.

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

register_task(Task, RegisteredTasks) ->
  Handle = erlang:unique_integer([positive, monotonic]),
  TaskWithHandle = Task#task{progress_handle = Handle},
  TaskProgress = #task_progress{task = TaskWithHandle},
  NewRegisteredTasks = RegisteredTasks#{Handle => TaskProgress},
  {TaskWithHandle, NewRegisteredTasks}.

report_progress(#task_progress{progress = Progress, task = Task,
    last_report_timestamp = LastTimestamp, last_reported_progress = LastProgress} = TaskProgress) ->
  Timestamp = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(Timestamp - LastTimestamp, native, seconds),
  % avoid spaming the report endpoint by reporting at most once per second and
  % only if more than 4% difference from last report
  case Progress >= LastProgress + 4 andalso Elapsed >= 1 of
    true ->
      Report = #{task_id => Task#task.task_id, progress => Progress, timestamp => Task#task.timestamp},
      'Elixir.Cloak.AirSocket':send_task_progress_report(Report),
      TaskProgress#task_progress{last_report_timestamp = Timestamp, last_reported_progress = Progress};
    false ->
      TaskProgress
  end.


%% -------------------------------------------------------------------
%% Anonymization of progress
%% -------------------------------------------------------------------

%% Due to a race condition we may have the case that the number of jobs is not set before we get the
%% first finished job.
anonymize_progress(#task_progress{number_of_jobs=0}) ->
  0;
anonymize_progress(#task_progress{number_of_jobs=Num, finished_jobs=Finished}) ->
  BucketReport = #bucket_report{
    label = #bucket_label{label = <<"progress">>},
    count = Finished,
    noisy_count = Finished,
    users_hash = crypto:hash(md4, <<"progress">>),
    noise_sd = 0
  },
  NoisyCount = case anonymizer:anonymize([BucketReport]) of
    [] ->
      0;
    [#bucket_report{noisy_count=NC}] ->
      % We don't want to have strange results, so make 0 and the maximum number of jobs a bound for it.
      max(0, min(NC, Num))
  end,
  round(100 * NoisyCount / Num).

update_progress_report(#task_progress{progress=OldProgress}=TaskProgress) ->
  AnonProgress = anonymize_progress(TaskProgress),
  % Ensure that the progress is monotonically increasing.
  NewProgress = max(OldProgress, AnonProgress),
  report_progress(TaskProgress#task_progress{progress=NewProgress}).
