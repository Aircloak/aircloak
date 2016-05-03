%% @doc Running jobs in the sandbox.
%%      The purpose of this gen_server is to run jobs on the sandbox.  It maintains a single list of jobs to
%%      execute and executes them sequentially in the sandbox.  To have a high degree of parallelism we use
%%      multiple instances of this gen_server.  The real interface module is {@link job_runner_sup} which
%%      chooses one of the job_runner's to execute a job.
%%
%%      Executing a job in the sandbox consists of the following steps:
%%
%%      1. sending the job and the data to the sandbox,
%%      2. waiting for the sandbox to finish execution, and
%%      3. receiving the result and processing the table insert actions.
-module(job_runner).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  name/1,
  execute/6,
  send_request_timeout/2
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
-include("sandbox_pb.hrl").

-type parameters() :: {user_id(), job_data_streamer:job_input()}.
-type callback_fun() :: fun((term(), {ok | error, #job_response{}}) -> any()).
-type get_parameters_fun() :: fun((term()) -> undefined | parameters()).

-export_type([
  parameters/0,
  callback_fun/0,
  get_parameters_fun/0
]).

-record(job, {
  request_id :: term(),
  request :: #task{},
  parameters :: parameters() | get_parameters_fun(),
  reporter_fun :: undefined | callback_fun(),
  created_at :: erlang:timestamp(),
  accumulator :: job_accumulator()
}).

-record(state, {
  runner_num :: non_neg_integer(),
  job_queue = queue:new() :: queue:queue(),
  active_job = false :: false | #job{},
  port :: port() | closed,
  timeout = undefined :: undefined | non_neg_integer(), % remaining job time limit in microseconds
  timer_ref = undefined :: undefined | timer:tref(),
  timer_start = undefined :: undefined | erlang:timestamp(),
  active_job_ref = undefined :: undefined | reference(),
  start_time = undefined :: undefined | erlang:timestamp(),
  run_jobs = 0 :: non_neg_integer()
}).

-define(RESTART_INTERVAL, 10000).  % restart every 10000 jobs


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link(RunnerNum) ->
  gen_server:start_link({via, gproc, name(RunnerNum)}, ?MODULE, [RunnerNum], []).

%% @doc Return the gproc name of a job_runner given it's number.
-spec name(non_neg_integer()) -> {n, l, {atom(), non_neg_integer()}}.
name(JobRunnerNum) ->
  {n, l, {job_runner, JobRunnerNum}}.

%% @doc Execute a given job request for the specified user.
%%      `ReporterFun' is the callback function that should receive the result of the job execution.
%%      Each task has its own request identifier (RequestId) which is used to cancel jobs in case of timeouts
%%      in a higher layer.
-spec execute(non_neg_integer(), #task{}, parameters() | get_parameters_fun(),
    job_accumulator(), term(), callback_fun()) -> ok.
execute(JobRunnerNum, Request, Parameters, Accumulator, RequestId, ReporterFun) ->
  Job = #job{
    request_id=RequestId,
    request=Request,
    parameters=Parameters,
    accumulator=Accumulator,
    reporter_fun=ReporterFun,
    created_at=os:timestamp()
  },
  gen_server:cast(gproc:where(name(JobRunnerNum)), {execute, Job}),
  ok.

%% @doc Inform the job_runner that all jobs with a given request identifier should be cancelled.
-spec send_request_timeout(non_neg_integer(), term()) -> ok.
send_request_timeout(JobRunnerNum, RequestId) ->
  gen_server:cast(gproc:where(name(JobRunnerNum)), {send_request_timeout, RequestId}),
  ok.


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

init([RunnerNum]) ->
  process_flag(trap_exit, true),
  State = #state{
    runner_num = RunnerNum,
    port = open_sandbox_port()
  },
  {ok, State, infinity}.

handle_call(_Message, From, State) ->
  ?ERROR("unknown call to job_runner from ~p", [From]),
  {noreply, State, infinity}.

handle_cast({execute, Job}, #state{active_job=false}=State) ->
  {noreply, execute_job(Job, State), infinity};
handle_cast({execute, Job}, #state{job_queue=JQ}=State) ->
  {noreply, State#state{job_queue=queue:in(Job, JQ)}, infinity};
handle_cast({send_request_timeout, RequestId}, #state{active_job=#job{request_id=RequestId}=Job}=State)
    when Job#job.reporter_fun /= undefined ->
  handle_cast({send_request_timeout, RequestId}, State#state{active_job=Job#job{reporter_fun=undefined}});
handle_cast({send_request_timeout, RequestId}, #state{job_queue=JQ}=State1) ->
  State2 = State1#state{job_queue=remove_request(RequestId, JQ)},
  {noreply, State2, infinity};
handle_cast(_Message, State) ->
  ?ERROR("unknown cast to job_runner"),
  {noreply, State, infinity}.

handle_info({timeout, ActiveJobRef}, State) ->
  {noreply, timeout_old_job(ActiveJobRef, State)};
handle_info({Port, {data, Data}}, #state{port=Port}=State1) ->
  State2 = collect_data_from_port(Data, State1),
  {noreply, State2, infinity};
% After each port exit message, we need to reopen a new port and stop using the old one
handle_info({Port, {exit_status, _}}, #state{port=Port, active_job=false}=State) ->
  {noreply, State#state{port=open_sandbox_port()}, infinity};
handle_info({Port, {exit_status, Status}}, #state{port=Port, active_job=Job}=State1) ->
  State2 = generate_error_reply({exit, Status}, Job, State1#state{port=open_sandbox_port()}),
  {noreply, State2, infinity};
handle_info({'EXIT', Port, _}, #state{port=Port, active_job=false}=State) ->
  {noreply, State#state{port=open_sandbox_port()}, infinity};
handle_info({'EXIT', Port, Reason}, #state{port=Port, active_job=Job}=State1) ->
  State2 = generate_error_reply(Reason, Job, State1#state{port=open_sandbox_port()}),
  {noreply, State2, infinity};
handle_info({SomePort, {exit_status, _}}, State) when is_port(SomePort) ->
  % When we kill_sandbox_port/1 a port, it will generate a corresponding shutdown message.
  % As we do not want to wait for the message to get delivered to continue processing, we
  % will receive such a message here with an unknown port.  Just ignore it.
  {noreply, State, infinity};
handle_info({SomePort, {data, _}}, State) when is_port(SomePort) ->
  % When we kill_sandbox_port/1 a port, some remaining data might still come in from the old port.
  % As we do not want to wait for the message to get delivered to continue processing, we
  % will receive such a message here with an unknown port.  Just ignore it.
  {noreply, State, infinity};
handle_info({'EXIT', SomePort, _}, State)  when is_port(SomePort) ->
  {noreply, State, infinity};
handle_info(_Message, State) ->
  ?WARN("unknown message to job_runner"),
  {noreply, State, infinity}.

terminate(Type, #state{port=Port}=State) when Port /= closed ->
  ?INFO("initiating shutdown of sandbox"),
  catch(port_command(Port, <<>>)),  %% ignore any error here
  terminate(Type, State#state{port=closed});
terminate(normal, _) ->
  ok;
terminate(_Abnormal, _) ->
  ?ERROR("abnormal termination of job_runner"),
  ok.

code_change(_, _, _) ->
  {error, not_implemented}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

open_sandbox_port() ->
  PortProgram = filename:join(code:priv_dir(cloak), "sandbox"),
  open_port({spawn_executable, PortProgram}, [{packet, 4}, use_stdio, binary, exit_status]).

kill_sandbox_port(Port) ->
  {os_pid, OsPid} = erlang:port_info(Port, os_pid),
  os:cmd("kill -KILL " ++ integer_to_list(OsPid)).

execute_job(#job{request=Request, parameters={_User, _Input}} = Job, #state{port=Port, run_jobs=N}=State) ->
  % this is job with static parameters, execute it directly
  ?REPORT_DURATION("job.queued", Job#job.created_at),
  NextState = create_timeout_timer(State#state{run_jobs=N+1, start_time=os:timestamp()}),
  port_command(
        Port,
        sandbox_pb:encode_jobinputmessagepb(#jobinputmessagepb{
              job_request=#jobrequestpb{
                code=Request#task.code,
                libraries=[#librarydatapb{name=Name, code=Code} || {Name, Code} <- Request#task.libraries],
                accumulator=Job#job.accumulator,
                task_time=task_time(Request#task.timestamp)
              }
            })
      ),
  cloak_metrics:count("job.started"),
  NextState#state{active_job=Job};
execute_job(#job{request_id=RequestId, parameters=GetParametersFun} = Job, State) when is_function(GetParametersFun, 1) ->
  % this is a job with dynamic parameters, invoke callback to get parameters first
  Parameters = try GetParametersFun(RequestId) catch _:_ -> undefined end,
  case Parameters of
    undefined ->
      % no more parameters for this request
      run_next(State);
    {_User, _JobInput} ->
      % add this job back in the queue
      NextJobQueue = queue:in(Job, State#state.job_queue),
      % execute job with static parameters
      NextJob = Job#job{parameters=Parameters},
      execute_job(NextJob, State#state{job_queue=NextJobQueue})
  end.

task_time(undefined) -> undefined;
task_time(Ts) when is_integer(Ts) ->
  % round time to a minute precision
  (Ts div 60) * 60.

collect_data_from_port(Data, #state{active_job=Job, port=Port}=State) ->
  case (catch {ok, sandbox_pb:decode_joboutputmessagepb(Data)}) of
    {ok, #joboutputmessagepb{function_call=#functioncallpb{}=FunctionCall}} ->
      {User, JobInput} = Job#job.parameters,
      ReturnData = sandbox_external_call:call(User, JobInput, FunctionCall),
      port_command(Port, sandbox_pb:encode_jobinputmessagepb(#jobinputmessagepb{data=ReturnData})),
      State;
    {ok, #joboutputmessagepb{get_next_batch=#getnextbatchpb{table_name=TableName, reset_stream=ResetStream}}} ->
      {User, JobInput} = Job#job.parameters,
      {NewInput, TableData} = ?MEASURE("job.read_data", job_data_streamer:get_next_batch(
          User, cloak_util:binarify(TableName), JobInput, ResetStream)),
      port_command(Port, sandbox_pb:encode_jobinputmessagepb(#jobinputmessagepb{table_data=TableData})),
      State#state{active_job=Job#job{parameters={User, NewInput}}};
    {ok, #joboutputmessagepb{job_response=#jobresponsepb{status='OK'}=JobResponsePB}} ->
      Properties = JobResponsePB#jobresponsepb.properties,
      Accumulator = JobResponsePB#jobresponsepb.accumulator,
      generate_success_reply(Properties, Accumulator, Job, State);
    {ok, #joboutputmessagepb{job_response=#jobresponsepb{status='ERROR', error=Error}}} ->
      generate_error_reply({sandbox_error, Error}, Job, State);
    _ ->
      generate_error_reply(bad_response, Job, State)
  end.

maybe_restart_and_run_next(#state{port=Port, run_jobs=?RESTART_INTERVAL}=State) ->
  kill_sandbox_port(Port),
  run_next(State#state{port=open_sandbox_port(), run_jobs=0});
maybe_restart_and_run_next(State) ->
  run_next(State).

run_next(#state{job_queue=JQ1}=State) ->
  case queue:out(JQ1) of
    {empty, JQ1} ->
      State#state{active_job=false};
    {{value, Job}, JQ2} ->
      execute_job(Job, State#state{job_queue=JQ2})
  end.

generate_success_reply(Properties, Accumulator,
    #job{request=Request, parameters={User, _}, request_id=RequestId, reporter_fun=ReporterFun} = _Job, State) ->
  add_job_runtime_metric(State),
  JobResponse = #job_response{
    user_id = User,
    task_id = Request#task.task_id,
    properties = convert_properties(Properties),
    accumulator = case Accumulator of
      undefined -> undefined;
      _ -> cloak_util:binarify(Accumulator)
    end
  },
  cloak_metrics:count("job.successful"),
  report_result(ReporterFun, RequestId, {ok, JobResponse}),
  maybe_restart_and_run_next(stop_timeout_timer(State)).

convert_properties(Properties) ->
  [#property{label=cloak_util:binarify(Label), value=convert_bucket_value(Value)}
      || #jobresponsepb_property{label=Label, value=Value} <- Properties].

convert_bucket_value(undefined) -> <<"<notice: undefined value>">>;
convert_bucket_value(Value) -> cloak_util:binarify(Value).

generate_error_reply(Reason,
    #job{request=Request, parameters={User, _}, request_id=RequestId, reporter_fun=ReporterFun}, State) ->
  JobResponse = #job_response{
    user_id = User,
    task_id = Request#task.task_id,
    properties = [#property{
          label = ?JOB_EXECUTION_ERROR,
          value = string_of_reason(Reason)
        }]
  },
  report_result(ReporterFun, RequestId, {error, JobResponse}),
  cloak_metrics:count("job.failed"),
  maybe_restart_and_run_next(stop_timeout_timer(State)).

report_result(undefined, _, _) -> ok;
report_result(ReporterFun, RequestId, Reply) ->
  ReporterFun(RequestId, Reply).

string_of_reason(Term) ->
  iolist_to_binary(io_lib:format("~p", [Term])).

remove_request(RequestId, List) ->
  queue:filter(fun(#job{request_id=Id}) -> RequestId /= Id end, List).


%% -------------------------------------------------------------------
%% Timeout handling
%% -------------------------------------------------------------------

% returns job timeout in miliseconds
load_timeout_from_conf() ->
  case cloak_conf:get_val(sandbox, max_time) of
    I when is_integer(I), I > 0 -> I
  end.

create_timeout_timer(#state{timer_ref=undefined, active_job_ref=undefined}=State) ->
  ActiveJobRef = make_ref(),
  {ok, TimerRef} = timer:send_after(load_timeout_from_conf(), {timeout, ActiveJobRef}),
  State#state{timer_ref=TimerRef, active_job_ref=ActiveJobRef}.

stop_timeout_timer(#state{timer_ref=TimerRef}=State) ->
  {ok, cancel} = timer:cancel(TimerRef),
  State#state{timer_ref=undefined, active_job_ref=undefined}.

timeout_old_job(AJRef, #state{port=Port, active_job_ref=AJRef, active_job=Job}=State) ->
  kill_sandbox_port(Port),
  cloak_metrics:count("job.timeout"),
  add_job_runtime_metric(State),
  generate_error_reply(job_timeout, Job, State#state{port=open_sandbox_port()});
timeout_old_job(_, State) ->
  State.


%% -------------------------------------------------------------------
%% Job runtime metrics
%% -------------------------------------------------------------------

add_job_runtime_metric(#state{start_time=StartTime}) ->
  cloak_metrics:histogram("job.duration", timer:now_diff(os:timestamp(), StartTime) div 1000).


%% -------------------------------------------------------------------
%% Testing table insert conversion
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

-define(run_sandbox_test(Code), ?run_sandbox_test(Code, [])).
-define(run_sandbox_test(Code, Libraries), ?run_sandbox_test(Code, Libraries, undefined)).
-define(run_sandbox_test(Code, Libraries, Accumulator), ?run_sandbox_test(Code, Libraries, Accumulator, undefined)).
-define(run_sandbox_test(Code, Libraries, Accumulator, Timestamp),
    (
      fun() ->
        ReqId = make_ref(),
        Me = self(),
        job_runner:execute(1,
              #task{
                task_id=1, prefetch=undefined, libraries=Libraries, code=Code, timestamp=Timestamp
              },
              {<<"user-1">>, []}, Accumulator, ReqId,
              fun(RId, Response) when RId =:= ReqId ->
                Me ! {job_done, Response}
              end
            ),
        receive
          {job_done, Response} ->
            Response
        after 500 -> timeout
        end
      end
    )()).

-define(job_response(Properties, Accumulator),
      #job_response{user_id= <<"user-1">>, task_id=1, properties=Properties, accumulator=Accumulator}
    ).

run_sandbox_test_() ->
{setup,
  fun() -> ok end,
  fun(_) -> ok end,
  [
    {"job runs", fun() ->
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("
            report_property('prop1', 'val1')
            report_property('prop2', 'val2')
          "),
      ?assertEqual(
            [
              {property, <<"prop1">>, <<"val1">>},
              {property, <<"prop2">>, <<"val2">>}
            ],
            lists:sort(Properties)
          )
    end},
    {"integer accumulator", fun() ->
      {ok, ?job_response(Properties1, Acc)} = ?run_sandbox_test("
            if accumulator == nil then
              report_property('prop1', 'val1')
              accumulator = 30
            else
              accumulator = 0
            end
          ", [], undefined),
      ?assertEqual([{property, <<"prop1">>, <<"val1">>}], lists:sort(Properties1)),
      {ok, ?job_response(Properties2, _)} = ?run_sandbox_test("report_property('prop2', accumulator + 12)",
          [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"42">>}], lists:sort(Properties2))
    end},
    {"boolean accumulator", fun() ->
      {ok, ?job_response(_, Acc)} = ?run_sandbox_test("accumulator = true", [], undefined),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("report_property('prop2', accumulator)", [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"true">>}], lists:sort(Properties))
    end},
    {"string accumulator", fun() ->
      {ok, ?job_response(_, Acc)} = ?run_sandbox_test("accumulator = '\"foobar\\''", [], undefined),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("report_property('prop2', accumulator)", [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"\"foobar'">>}], lists:sort(Properties))
    end},
    {"nil accumulator", fun() ->
      {ok, ?job_response(_, Acc)} = ?run_sandbox_test("accumulator = nil", [], undefined),
      ?assertEqual(undefined, Acc),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test(
            "if accumulator == nil then report_property('prop2', 'foo') end", [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"foo">>}], lists:sort(Properties))
    end},
    {"array accumulator", fun() ->
      {ok, ?job_response(_, Acc)} = ?run_sandbox_test("accumulator = {3, 0.14}", [], undefined),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test(
          "report_property('prop2', accumulator[1] + accumulator[2])", [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"3.14">>}], lists:sort(Properties))
    end},
    {"complex table", fun() ->
      {ok, ?job_response(_, Acc)} = ?run_sandbox_test(
            "
              accumulator = {}
              accumulator[{a='b', c='d'}] = {4, 5, 6}
            ", [], undefined),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test(
            "
              res = ''
              for k, v in pairs(accumulator) do
                for k1, v1 in pairs(k) do
                  res = res..k1..v1
                end
                for k2, v2 in pairs(v) do
                  res = res..k2..v2
                end
              end
              report_property('prop2', res)
            ",
            [], Acc),
      ?assertEqual([{property, <<"prop2">>, <<"abcd142536">>}], lists:sort(Properties))
    end},
    {"task_time", fun() ->
      Timestamp = cloak_util:timestamp_to_epoch(os:timestamp()),
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("
            report_property('time', task_time)
          ", [], undefined, Timestamp),
      ?assertEqual(
            [{property, <<"time">>, integer_to_binary((Timestamp div 60) * 60)}],
            Properties
          )
    end},
    {"nil task_time", fun() ->
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("
            report_property('time', task_time or 'nil')
          ", [], undefined, undefined),
      ?assertEqual([{property, <<"time">>, <<"nil">>}], Properties)
    end},
    {"to_date", fun() ->
      % 27.03.5015. 10:43:42
      Timestamp = 96098265822,
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test("
            time = to_date(task_time)
            report_property('time', table.concat({time.year, time.month, time.day, time.hour, time.min}, '/'))
          ", [], undefined, Timestamp),
      ?assertEqual(
            [{property, <<"time">>, <<"5015/3/27/10/43">>}],
            Properties
          )
    end},
    {"libraries are loaded", fun() ->
      {ok, ?job_response(Properties, _)} = ?run_sandbox_test(
            "foo(); bar();",
            [
              {<<"lib1">>, <<"function foo() report_property('prop1', 'val1') end">>},
              {<<"lib2">>, <<"function bar() report_property('prop2', 'val2') end">>}
            ]
          ),
      ?assertEqual(
            [
              {property, <<"prop1">>, <<"val1">>},
              {property, <<"prop2">>, <<"val2">>}
            ],
            lists:sort(Properties)
          )
    end},
    {"library syntax error", fun() ->
      {error, ?job_response(Properties, _)} = ?run_sandbox_test(
            "",
            [{<<"lib1">>, <<"library syntax error">>}]
          ),
      ?assertEqual(
            [{property, ?JOB_EXECUTION_ERROR, <<"{sandbox_error,\"cannot load library lib1\"}">>}],
            lists:sort(Properties)
          )
    end},
    {"library runtime error", fun() ->
      {error, ?job_response(Properties, _)} = ?run_sandbox_test(
            "",
            [{<<"lib1">>, <<"function foo() error('bar') end\nfoo()">>}]
          ),
      ?assertEqual(
            [{property, ?JOB_EXECUTION_ERROR, <<"{sandbox_error,\"[string \\\"lib1\\\"]:1: bar\"}">>}],
            lists:sort(Properties)
          )
    end},
    {"code syntax error", fun() ->
      {error, ?job_response(Properties, _)} = ?run_sandbox_test(
            "code syntax error",
            [{<<"lib1">>, <<"">>}]
          ),
      ?assertEqual(
            [{property, ?JOB_EXECUTION_ERROR, <<"{sandbox_error,\"[string \\\"task_code\\\"]:1: '=' expected near 'syntax'\"}">>}],
            lists:sort(Properties)
          )
    end},
    {"code runtime error", fun() ->
      {error, ?job_response(Properties, _)} = ?run_sandbox_test(
            "function foo() error('bar') end\nfoo()",
            [{<<"lib1">>, <<"">>}]
          ),
      ?assertEqual(
            [{property, ?JOB_EXECUTION_ERROR, <<"{sandbox_error,\"[string \\\"task_code\\\"]:1: bar\"}">>}],
            lists:sort(Properties)
          )
    end}
  ]
}.

-endif.
