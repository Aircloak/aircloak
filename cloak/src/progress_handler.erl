%% @doc This module implements a progress counter for tasks.
%%      It's purpose is to give analysts a way to see if the execution progresses and a hint how long it will
%%      take to finish processing the task.
-module(progress_handler).
-behaviour(gen_server).

-include("cloak.hrl").

%% API
-export([
  start_link/0,
  register_task/2,
  unregister_task/1,
  add_unfinished_jobs/2,
  get_progress/1,
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
  report_url = undefined :: string() | undefined, % the URL to report progress at
  last_reported_progress = 0 :: non_neg_integer(),
  last_report_timestamp = 0 :: non_neg_integer()
}).

-type state() :: dict:dict(binary(), #task_progress{}).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a task to be progress-supervised.
%%      In addition to registering this task in the progress supervisor it will also add the unique ID to the
%%      task record.
-spec register_task(#task{}, string()) -> #task{}.
register_task(Task, ReportUrl) ->
  gen_server:call(?MODULE, {register_task, Task, ReportUrl}).

%% @doc Unregister a progress-supervised task.
-spec unregister_task(#task{}) -> ok.
unregister_task(#task{progress_handle=Handle, progress_manager=ProgressMan}) ->
  gen_server:cast(ProgressMan, {unregister_task, Handle}).

%% @doc Set the number of jobs for the given task.
-spec add_unfinished_jobs(#task{}, non_neg_integer()) -> ok | {error, any()}.
add_unfinished_jobs(#task{progress_handle=undefined}, _NumOfJobs) ->
  % No progress handler registered: just ignore.
  ok;
add_unfinished_jobs(#task{progress_handle=Handle, progress_manager=ProgressMan}, NumOfJobs) ->
  gen_server:cast(ProgressMan, {add_unfinished_jobs, Handle, NumOfJobs}).

%% @doc Register that a job of a progress-supervised task finished.
-spec job_of_task_finished(#task{}) -> ok.
job_of_task_finished(#task{progress_handle=undefined}) ->
  % No progress handler registered: just ignore.
  ok;
job_of_task_finished(#task{progress_handle=Handle, progress_manager=ProgressMan}) ->
  gen_server:cast(ProgressMan, {job_of_task_finished, Handle}).

%% @doc Get the progress report for a given unique progress handle.
-spec get_progress(binary()) -> {ok, non_neg_integer()} | {error, string()}.
get_progress(Handle) ->
  case get_node_of_handle(Handle) of
    {ok, ProgressNode} ->
      gen_server:call({?MODULE, ProgressNode}, {get_progress, Handle});
    {error, _} ->
      {error, "invalid handle"}
  end.


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

-spec init(any()) -> {ok, state()}.
init(_) ->
  {ok, dict:new()}.

handle_call({register_task, Task, ReportUrl}, _From, RegisteredTasks) ->
  {NewTask, NewRegisteredTasks} = register_task(Task, ReportUrl, RegisteredTasks),
  {reply, NewTask, NewRegisteredTasks};
handle_call({get_progress, Handle}, _From, RegisteredTasks) ->
  case dict:find(Handle, RegisteredTasks) of
    error ->
      {reply, {error, "unknown task"}, RegisteredTasks};
    {ok, #task_progress{progress=Progress}} ->
      {reply, {ok, Progress}, RegisteredTasks}
  end.

handle_cast({add_unfinished_jobs, Handle, NumOfJobs}, RegisteredTasks) ->
  case dict:find(Handle, RegisteredTasks) of
    error ->
      ?ERROR("add_unfinished_jobs called for unknown handle ~p", [Handle]),
      {noreply, RegisteredTasks};
    {ok, #task_progress{number_of_jobs=OldNumOfJobs}=TaskProgress} ->
      NewTaskProgress = TaskProgress#task_progress{
        number_of_jobs = OldNumOfJobs + NumOfJobs,
        progress = 0  % reset the progress in case we had already finished processing of jobs on another node
      },
      NewTaskProgressWithUpdatedReport = update_progress_report(NewTaskProgress),
      {noreply, dict:store(Handle, NewTaskProgressWithUpdatedReport, RegisteredTasks)}
  end;
handle_cast({unregister_task, Handle}, RegisteredTasks) ->
  case dict:is_key(Handle, RegisteredTasks) of
    true ->
      {noreply, dict:erase(Handle, RegisteredTasks)};
    false ->
      ?WARNING("Cannot remove unknown handle from progress manager"),
      {noreply, RegisteredTasks}
  end;
handle_cast({job_of_task_finished, Handle}, RegisteredTasks) ->
  case dict:find(Handle, RegisteredTasks) of
    error ->
      ?WARNING("reported progress for a task unknown to the supervisor"),
      {noreply, RegisteredTasks};
    {ok, #task_progress{finished_jobs=Jobs}=TaskProgress} ->
      NewTaskProgress = TaskProgress#task_progress{finished_jobs=Jobs+1},
      NewTaskProgressWithUpdatedReport = update_progress_report(NewTaskProgress),
      {noreply, dict:store(Handle, NewTaskProgressWithUpdatedReport, RegisteredTasks)}
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

register_task(Task, ReportUrl, RegisteredTasks) ->
  Handle = create_unique_node_handle(RegisteredTasks),
  TaskWithHandle = Task#task{progress_handle=Handle, progress_manager=self()},
  TaskProgress = #task_progress{task=TaskWithHandle, report_url=ReportUrl},
  NewRegisteredTasks = dict:store(Handle, TaskProgress, RegisteredTasks),
  {TaskWithHandle, NewRegisteredTasks}.

report_progress(#task_progress{report_url=undefined}=TaskProgress) ->
  TaskProgress; % no one to report to
report_progress(#task_progress{report_url=ReportUrl, progress=Progress, task=Task,
    last_report_timestamp=LastTimestamp, last_reported_progress=LastProgress}=TaskProgress) ->
  Timestamp = cloak_util:timestamp_to_int(os:timestamp()),
  % avoid spaming the report url endpoint by reporting at most once per second and
  % only if more than 4% difference from last report
  case Progress >= LastProgress + 4 andalso Timestamp - LastTimestamp >= 1 * 1000 * 1000 of
    true ->
      Body = mochijson2:encode([
        {progress, Progress},
        {timestamp, Task#task.timestamp}
      ]),
      Request = {ReportUrl, [], "application/json", iolist_to_binary(Body)},
      httpc:request(post, Request, [], []),
      TaskProgress#task_progress{last_report_timestamp=Timestamp, last_reported_progress=Progress};
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


%% -------------------------------------------------------------------
%% Handle handling
%% -------------------------------------------------------------------

get_node_of_handle(Handle) ->
  [NodeString, _] = string:tokens(binary_to_list(Handle), "#"),
  % We shouldn't do list_to_atom/1 directly as the number of atoms is limited.  So detect if NodeString is a
  % valid string of a node.  So first get a list of valid nodes (for sake of simplicity, we just get all nodes
  % of the current cluster and convert those to a string to see if they match).  This mechanism is a little
  % bit ugly but prevents that someone can DoS us by sending in random strings.
  Nodes = [atom_to_list(Node) || Node <- nodes([this, visible])],
  case lists:member(NodeString, Nodes) of
    true ->
      {ok, list_to_atom(NodeString)};
    false ->
      {error, unknown_node}
  end.

create_unique_node_handle(RegisteredTasks) ->
  Now = erlang:unique_integer([positive, monotonic]),
  Handle = iolist_to_binary([atom_to_list(node()), "#", integer_to_list(Now)]),
  % Due to the semantics of erlang:now/1 we should have generated an unique key.  Just assert this here.
  false = dict:is_key(Handle, RegisteredTasks),
  Handle.
