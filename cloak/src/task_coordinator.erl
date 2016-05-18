%% @doc Coordinates the execution of a task.
-module(task_coordinator).
-behaviour(queued_worker).

%% API
-export([
  run_task/1
]).

%% queued_worker callbacks
-export([
  run_job/1,
  on_failure/2
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Executes a task asynchronously.
-spec run_task(#task{}) -> ok.
run_task(Task) ->
  queued_worker:run(?MODULE, Task).


%% -------------------------------------------------------------------
%% queued_worker callbacks
%% -------------------------------------------------------------------

%% @hidden
run_job(Task) ->
  StartTime = erlang:timestamp(),
  % TODO: implement the actual task execution
  % We need to:
  %   - process the AST of the query
  %   - reassemble the AST into a SQL query
  %   - execute the query and retrieve the working data set
  %   - pre-process the data set into buckets
  %   - anonymize the resulting buckets
  %   - post-process the buckets
  %   - convert the anonymized buckets into a table: done
  %   - send the final result to its destination
  % For now, return an empty result set.
  Result = {buckets, [], []},
  progress_handler:unregister_task(Task),
  cloak_metrics:count("task.successful"),
  #task{task_id = TaskId, result_destination = ResultDestination} = Task,
  ?MEASURE("task.send_result", result_sender:send_result(TaskId, ResultDestination, Result)),
  ?REPORT_DURATION("task.total", StartTime),
  ok.

% TODO: This should be called back by the queued_worker in case of an error or timeout
on_failure(Task, Reason) ->
  ?ERROR("task_coordinator failure: ~p", [Reason]),
  cloak_metrics:count("task.failure"),
  progress_handler:unregister_task(Task),
  #task{task_id = TaskId, result_destination = ResultDestination} = Task,
  Result = {error, <<"task execution failed">>},
  ?MEASURE("task.send_result", result_sender:send_result(TaskId, ResultDestination, Result)).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------
