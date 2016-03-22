%% @doc The `db_job' module provides a queueing mechanism that puts
%%      an upper bound on the concurrency of database operations.
%%
%%      It internally distinguishes between three job types:
%%
%%      - streaming
%%      - write
%%      - generic
%%
%%      Streaming database jobs are related to streaming queries.
%%      Since we are trying to approximate real-timeness for streaming
%%      tasks, these have the highest priority. If there are streaming
%%      job requests in the queue, these will run first.
%%
%%      Write jobs are used when inserting user data into the database.
%%      They have higher priority than the generic database operations,
%%      which are used for anything not related to inserts or streaming
%%      queries. The ratio of write to generic jobs is given by
%%      ?WRITE_CREDITS / ?GENERIC_CREDITS
-module(db_job).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  generic/1,
  write/2,
  streaming/1
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

-define(WRITE_CREDITS, 20).
-define(GENERIC_CREDITS, 5).
-define(WRITE_QUEUE_LEN, 250).

-record(state, {
  generic_credits = ?GENERIC_CREDITS :: non_neg_integer(),
  write_credits = ?WRITE_CREDITS :: non_neg_integer(),

  concurrent_requests = 0 :: non_neg_integer(),
  max_concurrency = 5 :: non_neg_integer(),

  % We support three distinct queues.
  % In order to not have to rely on
  % the queue module to get the queue
  % lengths (O(n)), we also keep the
  % queue length separately, at the risk
  % of not being able to maintain these
  % correctly...
  streaming_queue = queue:new() :: queue:queue(),
  streaming_queue_length = 0 :: non_neg_integer(),
  write_queue = queue:new() :: queue:queue(),
  write_queue_length = 0 :: non_neg_integer(),
  % We limit the write queue, to ensure it doesn't
  % grow uncontrollably. The other queues are internal
  % queues, so we have to process them.
  max_write_queue_length = ?WRITE_QUEUE_LEN :: non_neg_integer(),
  generic_queue = queue:new() :: queue:queue(),
  generic_queue_length = 0 :: non_neg_integer()
}).

-include("cloak.hrl").

-type enqueue_response() ::
% The call blocks while the caller is waiting in the queue.
% When at the head of the queue and ready to be processed,
% ok is returned
    ok
% queue_full is returned immediately if the queue is at capacity.
% The caller needs to handle this case, and deal with it gracefully.
  | {error, queue_full}.
-type db_job() :: fun(() -> any()).

-export_type([
  enqueue_response/0,
  db_job/0
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Start the node local `db_job' instance. This service
%%      should not be started manually, but rather through the
%%      `db_job_queue_sup' supervisor, as there are auxiliary
%%      queueing services that are also required for this
%%      service to operate.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  ?DEBUG("Starting queue service"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Blocks until a generic database operation is allowed
%%      to execute. The generic database operation should be
%%      performed inside the DB job closure passed to the
%%      function, as the database slot is returned back to
%%      the pool once the job has completed.
-spec generic(db_job()) -> any() | enqueue_response().
generic(Job) ->
  enqueue(node(), generic_queue, Job).

%% @doc Blocks until a write database operation is allowed
%%      to execute on the given node. The write database operation should be
%%      performed inside the DB job closure passed to the
%%      function, as the database slot is returned back to
%%      the pool once the job has completed.
-spec write(node(), db_job()) -> any() | enqueue_response().
write(Node, Job) ->
  enqueue(Node, write_queue, Job).

%% @doc Blocks until a streaming database operation is allowed
%%      to execute. The streaming database operation should be
%%      performed inside the DB job closure passed to the
%%      function, as the database slot is returned back to
%%      the pool once the job has completed.
-spec streaming(db_job()) -> any() | enqueue_response().
streaming(Job) ->
  enqueue(node(), streaming_queue, Job).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init([]) ->
  MaxConcurrency = pool_size(),
  ?INFO("Using db queue concurrency of ~p based on dynamic host params", [MaxConcurrency]),
  {ok, #state{max_concurrency = MaxConcurrency}}.

handle_call({enqueue, QueueName}, From, State) ->
  case enqueue_in_state(QueueName, From, State) of
    {ok, StateWithItem} ->
      StateWithWork = do_work_if_applicable(StateWithItem),
      {noreply, StateWithWork};
    {error, queue_full} = Error ->
      {reply, Error, State}
  end.


% NOTE: This is currently only used by tests.
%       In the future it might be made a public
%       API to allow external systems to set the
%       desired concurrency level.
handle_cast({max_concurrency, Concurrency}, State) ->
  UpdatedState = State#state{max_concurrency = Concurrency},
  ?INFO("Updated max concurrency to ~p", [Concurrency]),
  {noreply, UpdatedState};
handle_cast(job_done, #state{concurrent_requests=ConcurrentRequests}=State) ->
  ?DEBUG("Job done message received"),
  ReducedCountState = State#state{concurrent_requests = ConcurrentRequests - 1},
  UpdatedState = do_work_if_applicable(ReducedCountState),
  {noreply, UpdatedState}.

handle_info(Msg, State) ->
  ?ERROR("Unexpected message in db_job server: ~p", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

pool_size() ->
  {CPUCountMultiplier, FixedSize} = cloak_conf:get_val(cloak_db, pool_size),
  FixedSize + get_cpu_count_relative_size(CPUCountMultiplier).

-spec get_cpu_count_relative_size(float()) -> pos_integer().
get_cpu_count_relative_size(Multiplier) ->
  CPUCount = case erlang:system_info(logical_processors_available) of
    unknown -> 2; % if we can't determine CPU count, assume 2
    Count -> Count
  end,
  round(Multiplier * CPUCount).

enqueue(Node, Queue, Task) ->
  ?DEBUG("Enqueueing a db job request in queue: ~p (on node ~p)", [Queue, Node]),
  % We create a secondary handling process here to prevent pool starvation.
  % This unfortunately adds some complexity and overhead, but is a necessity.
  % The main problem we want to solve is the calling process crashing or being
  % killed while it holds a DB processing token. This would result in the
  % token not being returned, and the processing pool thereby getting depleted.
  Self = self(),
  {JobMonitor, MonitorRef} = spawn_monitor(fun() ->
        acquire_db_job_allowance(Node, Queue, Self)
      end),
  try
    BinaryQueueName = cloak_util:binarify(Queue),
    DurationMetricName = <<"db_operation.duration.", BinaryQueueName/binary >>,
    case Node =:= node() of
      true ->
        cloak_metrics:count(<<"db_operation.local">>);
      false ->
        cloak_metrics:count(<<"db_operation.remote">>)
    end,
    cloak_metrics:count(<<"db_operation.started.", BinaryQueueName/binary>>),
    Result = receive
      {JobMonitor, ok} ->
        try ?MEASURE(DurationMetricName, Task())
        after JobMonitor ! {Self, done}
        end;
      {'DOWN', MonitorRef, process, JobMonitor, Reason} ->
        cloak_metrics:count(<<"db_operation.error.", BinaryQueueName/binary>>),
        ?CRITICAL("DB job queue monitor failed with ~p. No DB action taken", [Reason]),
        {error, Reason};
      {JobMonitor, {error, _} = Error} ->
        cloak_metrics:count(<<"db_operation.error.", BinaryQueueName/binary>>),
        Error
    end,
    cloak_metrics:count(<<"db_operation.finished.", BinaryQueueName/binary>>),
    Result
  after
    erlang:demonitor(MonitorRef, [flush])
  end.

acquire_db_job_allowance(Node, QueueName, WaitingPid) ->
  % So we are informed if the process is killed
  MonitorRef = monitor(process, WaitingPid),
  QueueMetricName = <<"db_operation.queued.", (cloak_util:binarify(QueueName))/binary >>,
  case ?MEASURE(QueueMetricName, gen_server:call({?MODULE, Node}, {enqueue, QueueName}, infinity)) of
    ok ->
      WaitingPid ! {self(), ok},
      receive
        {WaitingPid, done} ->
          % The process is done with the token, so we can return it again
          gen_server:cast({?MODULE, Node}, job_done);
        {'DOWN', MonitorRef, process, WaitingPid, _} ->
          % The monitored process dies, release the token
          gen_server:cast({?MODULE, Node}, job_done)
      end;
    {error, _} = Error ->
      % No job token was given, hence the only thing
      % we need to do is inform the caller, and terminate.
      WaitingPid ! {self(), Error}
  end.

do_work_if_applicable(State) ->
  {QueuedFroms, UpdatedState} = collect_work(State),
  [gen_server:reply(QueuedFrom, ok) || QueuedFrom <- QueuedFroms],
  UpdatedState.

-spec collect_work(#state{}) -> {[term()], #state{}}.
collect_work(State) ->
  collect_work(State, []).

% If we are already doing as much
% concurrent work as we can, then we also stop.
collect_work(#state{concurrent_requests=ConcurrentRequests, max_concurrency=MaxAllowed}=State, WorkAcc)
    when ConcurrentRequests >= MaxAllowed ->
  ?DEBUG("Not allowed to start a db job, we have reached the maximum concurrency level"),
  {WorkAcc, State};
% There are several cases where we need to reset the job credits
% for writing and generic jobs. The scenarios are as follows:
% - no more credits of any kind
% - there are only jobs of one type, but no more credit
%   of that type. If so, reset all credits
collect_work(#state{write_credits=0, generic_credits=0}=State, WorkAcc) ->
  ?DEBUG("No more credits of any kind, replenish"),
  collect_work(reset_credits(State), WorkAcc);
collect_work(#state{write_queue_length=WQL, generic_queue_length=0,
    write_credits=0}=State, WorkAcc) when WQL > 0 ->
  ?DEBUG("No more write credits, but write work, and no generic work"),
  collect_work(reset_credits(State), WorkAcc);
collect_work(#state{write_queue_length=0, generic_queue_length=GQL,
    generic_credits=0}=State, WorkAcc) when GQL > 0 ->
  ?DEBUG("No more generic credits, but generic work to do, and no write work"),
  collect_work(reset_credits(State), WorkAcc);
% If we have no work to be done, we stop
collect_work(#state{streaming_queue_length=0, write_queue_length=0, generic_queue_length=0}=State, WorkAcc) ->
  ?DEBUG("No more db jobs to start"),
  {WorkAcc, State};
collect_work(#state{streaming_queue_length=QueueLength, streaming_queue=Queue}=State, WorkAcc)
    when QueueLength > 0 ->
  {ok, {QueuedFrom, DequeuedQueue}} = dequeue_job(Queue),
  UpdatedState = State#state{
    streaming_queue = DequeuedQueue,
    streaming_queue_length = QueueLength - 1
  },
  collect_work(increment_concurrent_jobs(UpdatedState), [QueuedFrom | WorkAcc]);
collect_work(#state{write_queue_length=QueueLength, write_credits=Credits, write_queue=Queue}=State, WorkAcc)
    when QueueLength > 0 andalso Credits > 0 ->
  {ok, {QueuedFrom, DequeuedQueue}} = dequeue_job(Queue),
  UpdatedState = State#state{
    write_queue = DequeuedQueue,
    write_queue_length = QueueLength - 1,
    write_credits = Credits - 1
  },
  collect_work(increment_concurrent_jobs(UpdatedState), [QueuedFrom | WorkAcc]);
collect_work(#state{generic_queue_length=QueueLength, generic_credits=Credits, generic_queue=Queue}=State, WorkAcc)
    when QueueLength > 0 andalso Credits > 0 ->
  {ok, {QueuedFrom, DequeuedQueue}} = dequeue_job(Queue),
  UpdatedState = State#state{
    generic_queue = DequeuedQueue,
    generic_queue_length = QueueLength - 1,
    generic_credits = Credits - 1
  },
  collect_work(increment_concurrent_jobs(UpdatedState), [QueuedFrom|WorkAcc]).

reset_credits(State) ->
  State#state{
    generic_credits = ?GENERIC_CREDITS,
    write_credits = ?WRITE_CREDITS
  }.

dequeue_job(Queue) ->
  case queue:out(Queue) of
    {{value, QueuedFrom}, ReducedQueue} ->
      ?DEBUG("Allowing a job to execute"),
      {ok, {QueuedFrom, ReducedQueue}};
    {empty, _} ->
      ?CRITICAL("DB job queue is broken. Trying to dequeue empty streaming queue"),
      {error, empty_queue}
  end.

increment_concurrent_jobs(#state{concurrent_requests=ConcurrentRequests}=State) ->
  State#state{concurrent_requests = ConcurrentRequests + 1}.

enqueue_in_state(streaming_queue, From,
    #state{streaming_queue=Queue, streaming_queue_length=QueueLength}=State) ->
  {ok, State#state{streaming_queue=queue:in(From, Queue), streaming_queue_length=QueueLength+1}};
enqueue_in_state(write_queue, _From,
    #state{write_queue_length=Max, max_write_queue_length=Max}) ->
  {error, queue_full};
enqueue_in_state(write_queue, From,
    #state{write_queue=Queue, write_queue_length=QueueLength}=State) ->
  {ok, State#state{write_queue=queue:in(From, Queue), write_queue_length=QueueLength+1}};
enqueue_in_state(generic_queue, From,
    #state{generic_queue=Queue, generic_queue_length=QueueLength}=State) ->
  {ok, State#state{generic_queue=queue:in(From, Queue), generic_queue_length=QueueLength+1}}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

queue_test_() ->
  {
    foreach,
    fun() -> ok end,
    fun(_) -> ok end,
    [
      {"It should allows tasks to execute", fun() ->
          FutureOperations = execute_operations([
                write,
                generic,
                streaming
              ]),
          CompletedOperations = await_operations(FutureOperations),
          ?assertEqual(3, length(CompletedOperations)),
          assert_all_ok(CompletedOperations),
          Operations = lists:sort(commands(CompletedOperations)),
          ?assertEqual([generic, streaming, write], Operations)
      end},

      {"DB access tokens should be released on client death", fun() ->
        % This test will exercise some different scenarios.
        % We will add some generic jobs that we have implode as soon
        % as they are scheduled, and we also add a generic job that
        % we kill before it has even been scheduled.
        % After these kamikaze jobs we also add some normal jobs
        % that should go through normally.
        % Because the concurrency of the queue is set to 1,
        % the normal jobs will only go through if in fact the
        % token reclaiming mechanism works.
        set_max_concurrency(1),
        Blocks = block_queue(1),
        DestructibleJobs = [destructible || _ <- lists:seq(1,5)],
        SelfDestructOperations = execute_operations(DestructibleJobs),
        GenericJobs = [generic || _ <- lists:seq(1,5)],
        FutureOperations = execute_operations(GenericJobs),
        % We kill the first pid even before it has been given the
        % db access token.
        [DestroyablePid] = execute_operations([generic]),
        erlang:exit(DestroyablePid, kill),
        % Ok, let the party start.
        remove_block(Blocks),
        % Now all the jobs that are self-destructible should
        % be scheduled, and then implode.
        % This should not prevent our normal generic jobs from going through.
        [DestructibleJobPid ! self_destruct || DestructibleJobPid <- SelfDestructOperations],
        CompletedOperations = await_operations(FutureOperations),
        ?assertEqual(5, length(CompletedOperations)),
        assert_all_ok(CompletedOperations)
      end}
    ]
  }.

collect_work_test_() ->
  [
    {"It should return an empty list of tasks when there is nothing to schedule", fun() ->
      ?assertMatch({[], _}, collect_work(#state{}))
    end},

    {"It should schedule no work even if there is, if concurrency level has been exceeded", fun() ->
      State = #state{},
      MaxedOutState = State#state{
        concurrent_requests = 10,
        max_concurrency = 10
      },
      {ok, EnqueuedState} = enqueue_in_state(streaming_queue, fake_from, MaxedOutState),
      ?assertMatch({[], _}, collect_work(EnqueuedState))
    end},

    {"It should reset credits if out of credits", fun() ->
      State = #state{},
      CreditlessState = State#state{
        generic_credits = 0,
        write_credits = 0
      },
      {_, UpdatedState} = collect_work(CreditlessState),
      ?assertEqual(?WRITE_CREDITS, UpdatedState#state.write_credits),
      ?assertEqual(?GENERIC_CREDITS, UpdatedState#state.generic_credits)
    end},

    {"If out of generic credits, but only generic jobs, reset credits", fun() ->
      State = #state{},
      CreditlessState = State#state{
        generic_credits = 0,
        write_credits = 1
      },
      {ok, StateWithWork} = enqueue_in_state(generic_queue, fake_from, CreditlessState),
      {[fake_from], UpdatedState} = collect_work(StateWithWork),
      ?assertEqual(?WRITE_CREDITS, UpdatedState#state.write_credits),
      ?assertEqual(?GENERIC_CREDITS - 1, UpdatedState#state.generic_credits)
    end},

    {"If out of write credits, but only write jobs, reset credits", fun() ->
      State = #state{},
      CreditlessState = State#state{
        generic_credits = 1,
        write_credits = 0
      },
      {ok, StateWithWork} = enqueue_in_state(write_queue, fake_from, CreditlessState),
      {[fake_from], UpdatedState} = collect_work(StateWithWork),
      ?assertEqual(?WRITE_CREDITS - 1, UpdatedState#state.write_credits),
      ?assertEqual(?GENERIC_CREDITS, UpdatedState#state.generic_credits)
    end},

    {"Should decrement queue counters when scheduling jobs", fun() ->
      {ok, State} = enqueue_in_state(write_queue, fake_from, #state{}),
      {ok, State1} = enqueue_in_state(generic_queue, fake_from, State),
      {ok, State2} = enqueue_in_state(streaming_queue, fake_from, State1),
      #state{
        write_queue_length = WriteQueueLength,
        generic_queue_length = GenericQueueLength,
        streaming_queue_length = StreamingQueueLength
      } = State2,
      ?assertEqual(1, WriteQueueLength),
      ?assertEqual(1, GenericQueueLength),
      ?assertEqual(1, StreamingQueueLength),
      {[fake_from, fake_from, fake_from], ProcessedState} = collect_work(State2),
      #state{
        write_queue_length = WriteQueueLengthPostDecrement,
        generic_queue_length = GenericQueueLengthPostDecrement,
        streaming_queue_length = StreamingQueueLengthPostDecrement
      } = ProcessedState,
      ?assertEqual(0, WriteQueueLengthPostDecrement),
      ?assertEqual(0, GenericQueueLengthPostDecrement),
      ?assertEqual(0, StreamingQueueLengthPostDecrement)
    end},

    {"Should increment concurrent jobs counters when scheduling jobs", fun() ->
      InitialState = #state{},
      ?assertEqual(0, InitialState#state.concurrent_requests),
      {ok, StateWithWork} = enqueue_in_state(write_queue, fake_from, #state{}),
      {_, ProcessedState} = collect_work(StateWithWork),
      ?assertEqual(1, ProcessedState#state.concurrent_requests)
    end},

    {"Should prioritise streaming jobs over others", fun() ->
      InitialState = (#state{})#state{
        max_concurrency = 4
      },
      {ok, StateWithWork1} = enqueue_in_state(streaming_queue, stream_from, InitialState),
      {ok, StateWithWork2} = enqueue_in_state(write_queue, write_from, StateWithWork1),
      {ok, StateWithWork3} = enqueue_in_state(streaming_queue, stream_from, StateWithWork2),
      {ok, StateWithWork4} = enqueue_in_state(write_queue, write_from, StateWithWork3),
      {ReversedJobs, _} = collect_work(StateWithWork4),
      ?assertEqual([stream_from, stream_from, write_from, write_from],
          lists:reverse(ReversedJobs))
    end},

    {"Should balance write and generic jobs", fun() ->
      Write = [write || _ <- lists:seq(1,1000*?WRITE_CREDITS)],
      Generic = [generic || _ <- lists:seq(1,1000*?GENERIC_CREDITS)],
      HighConcurrencyState = (#state{})#state{
        max_concurrency = length(Write ++ Generic),
        max_write_queue_length = length(Write)
      },
      StateWithJobs = lists:foldl(fun(Job, StateAcc) ->
            {ok, UpdatedStateAcc} = case Job of
              write -> enqueue_in_state(write_queue, write_from, StateAcc);
              generic -> enqueue_in_state(generic_queue, generic_from, StateAcc)
            end,
            UpdatedStateAcc
          end, HighConcurrencyState, Write ++ Generic),
      {ReversedJobs, _} = collect_work(StateWithJobs),
      Jobs = lists:reverse(ReversedJobs),
      ExpectedPattern =
          [write_from || _ <- lists:seq(1, ?WRITE_CREDITS)] ++
          [generic_from || _ <- lists:seq(1, ?GENERIC_CREDITS)],
      Partitions = partition_by(length(ExpectedPattern), Jobs),
      lists:foreach(fun(Partition) ->
            ?assertEqual(ExpectedPattern, Partition)
          end, Partitions)
    end}
  ].

create_full_write_state() ->
  lists:foldl(fun(_, StateAcc) ->
        {ok, UpdatedStateAcc} = enqueue_in_state(write_queue, from, StateAcc),
        UpdatedStateAcc
      end, #state{}, lists:seq(1, ?WRITE_QUEUE_LEN)).

% Validate that we can not exceed the maximum threshold of
% jobs in the write queue.
enqueue_in_state_test_() ->
  [
    {"Should reject write requests when queue is full", fun() ->
      FullState = create_full_write_state(),
      ?assertEqual({error, queue_full}, enqueue_in_state(write_queue, from, FullState))
    end},

    {"Should increase counters when work is added", fun() ->
      InitialState = #state{},
      {ok, State} = enqueue_in_state(write_queue, fake_from, InitialState),
      {ok, State1} = enqueue_in_state(generic_queue, fake_from, State),
      {ok, FinalState} = enqueue_in_state(streaming_queue, fake_from, State1),
      ?assertEqual(InitialState#state.write_queue_length + 1,
          FinalState#state.write_queue_length),
      ?assertEqual(InitialState#state.generic_queue_length + 1,
          FinalState#state.generic_queue_length),
      ?assertEqual(InitialState#state.streaming_queue_length + 1,
          FinalState#state.streaming_queue_length)
    end}
  ].

partition_by(_Num, []) -> [];
partition_by(Num, Operations) ->
  [lists:sublist(Operations, Num) | partition_by(Num, lists:nthtail(Num, Operations))].

execute_operations(Operations) ->
  Self = self(),
  Node = node(),
  ExecuteFun = fun(Operation) ->
    spawn(fun() ->
          Response = case Operation of
            write -> ?MODULE:write(Node, ?WRAP(ok));
            destructible -> ?MODULE:generic(fun() ->
                  receive self_destruct ->
                    erlang:exit(normal)
                  end
                end);
            _ -> ?MODULE:Operation(?WRAP(ok))
          end,
          Self ! {result, self(), {Operation, Response}}
        end)
  end,
  [ExecuteFun(Operation) || Operation <- Operations].

await_operations(Pids) ->
  await_operations(sets:from_list(Pids), []).

await_operations(PidsSet, AccumulatedResponses) ->
  case sets:size(PidsSet) of
    0 -> lists:reverse(AccumulatedResponses);
    _ ->
      receive
        {result, Pid, Result} ->
          ?assert(sets:is_element(Pid, PidsSet)),
          await_operations(sets:del_element(Pid, PidsSet), [Result|AccumulatedResponses]);
        Other ->
          ?debugFmt("Unexpected response while waiting for operations to complete: ~p", [Other]),
          ?assert(false)
      end
  end.

responses(CompletedOperations) ->
  [Response || {_Operation, Response} <- CompletedOperations].

commands(CompletedOperations) ->
  [Operation || {Operation, _Response} <- CompletedOperations].

block_queue(NumberOfBlocks) ->
  Self = self(),
  BlockFun = ?WRAP(streaming(?WRAP(receive {Self, unblock} -> ok end))),
  % We add lot of blocking DB operations here (10 * desired blocks).
  % The reason is that this adds some minor delay that allows
  % the scheduled test jobs to get into the queue reliably
  % before the block is removed.
  [spawn(BlockFun) || _ <- lists:seq(1, NumberOfBlocks*10)].

remove_block(Blocks) ->
  [Pid ! {self(), unblock} || Pid <- Blocks].

assert_all_ok(CompletedOperations) ->
  ?assert(lists:all(fun(ok) -> true; (_) -> false end, responses(CompletedOperations))).

%% NOTE: This function is currently only used in tests.
%%       Going forward we might want to expose it in order
%%       to allow external systems to set the concurrency
%%       level.
%%
%%       Sets the total number of concurrently allowed database
%%       operations. This can be tweaked at run-time to change the
%%       performance characteristics of the system
-spec set_max_concurrency(non_neg_integer()) -> ok.
set_max_concurrency(Concurrency) ->
  gen_server:cast(?MODULE, {max_concurrency, Concurrency}).

-endif.
