%% @doc Represents a worker JavaScript virtual machine, implemented as a gen_server wrapper
%%      over an external Erlang port that implements a JavaScript sandbox. The worker is part
%%      of a pool managed by the js_vm_sup module and can be used to execute JavaScript code.
%%
%%      The current implementation doesn't enforce memory limits on the JavaScript, since we're
%%      currently not experiencing any problems with memory consumption, and we're not sure which
%%      limit should be chosen.
-module(js_vm_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include_lib("aircloak_common/include/elixir_logger.hrl").

%% poolboy_worker callbacks
-export([
  start_link/1,
  call/3
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

-record(state, {
  vm :: port()
}).

%% @doc Starts the worker process.
-spec start_link([binary()]) -> {ok, pid()} | {error, any()}.
start_link(JSModules) ->
  gen_server:start_link(?MODULE, JSModules, []).

%% @doc Executes the specified function with the supplied and returns the result.
-spec call(pid(), binary(), [any()]) -> {ok, any()} | {error, any()}.
call(Worker, Function, Arguments) ->
  gen_server:call(Worker, {call, Function, Arguments}, infinity).

%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

init(JSModules) when is_list(JSModules) ->
  VM = open_sandbox(),
  lists:foreach(fun (JSModule) ->
    {ok, JSContent} = file:read_file(JSModule),
    {ok, _} = evaluate_js(VM, filename:basename(JSModule), JSContent)
  end, JSModules),
  {ok, #state{vm = VM}}.

handle_call({call, Func, Args}, _From, #state{vm = VM} = State) ->
  {reply, call_js(VM, Func, Args), State};
handle_call(Message, _From, State) ->
  ?ERROR("unknown call to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

handle_cast(Message, State) ->
  ?ERROR("unknown cast to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

handle_info({'EXIT', Port, ExitCode}, #state{vm = Port} = State) ->
  {stop, {js_worker_crash, ExitCode}, State};
handle_info(Message, State) ->
  ?ERROR("unknown info to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

terminate(normal, #state{vm = VM}) ->
  close_sandbox(VM),
  ok;
terminate(shutdown, State) ->
  terminate(normal, State);
terminate(Reason, State) ->
  ?ERROR("JS worker terminated with reason: ~p", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Sandbox actions.
-define(STOP, 0).
-define(EVALUATE, 1).
-define(CALL, 2).

% Sandbox supported types.
-define(UNDEFINED, 0).
-define(NULL, 1).
-define(BOOLEAN, 2).
-define(INT32, 3).
-define(DOUBLE, 4).
-define(STRING, 5).

-spec open_sandbox() -> port().
open_sandbox() ->
  process_flag(trap_exit, true),
  Binary = code:priv_dir(air) ++ "/js_sandbox/js_sandbox",
  open_port({spawn_executable, Binary}, [{packet, 4}, {args, []}, use_stdio, binary]).

-spec close_sandbox(port()) -> ok.
close_sandbox(Port) ->
  port_command(Port, message(?STOP, <<>>, <<>>)),
  receive
    {'EXIT', Port, normal} -> ok
  end.

-spec evaluate_js(port(), string() | binary(), binary()) -> {ok | error, atom() | number() | binary()}.
evaluate_js(Port, ScriptName, ScriptContent) ->
  port_command(Port, message(?EVALUATE, ScriptName, ScriptContent)),
  listen(Port).

-spec call_js(port(), string() | binary(), [atom() | number() | binary()]) -> {ok | error, atom() | number() | binary()}.
call_js(Port, FunctionName, Arguments) ->
  port_command(Port, message(?CALL, FunctionName, pack_arguments(Arguments))),
  listen(Port).

% Packs a request into an input message for the sandbox port.
-spec message(integer(), string() | binary(), iodata()) -> [binary() | string() | iodata()].
message(Action, Name, Body) when is_list(Name) ->
  message(Action, list_to_binary(Name), Body);
message(Action, Name, Body) when is_integer(Action), byte_size(Name) < 256 ->
  [<<Action, (byte_size(Name)):8>>, Name, Body].

% Unpacks an output message from the sandbox port into a result.
-spec parse_result(binary()) -> {ok | error, atom() | number() | binary()}.
parse_result(<<Success:8, Data/binary>>) ->
  Status = case Success of
    0 -> error;
    1 -> ok
  end,
  {Status, parse_data(Data)}.

% Erlang doesn't know about non-finite numbers, we need to handle those cases ourselves.
-spec decode_double(binary()) -> atom() | float().
decode_double(<<0:48, 16#F:4, 0:4, 0:1, 16#7F:7>>) ->
  'Inf';
decode_double(<<0:48, 16#F:4, 0:4, 1:1, 16#7F:7>>) ->
  '-Inf';
decode_double(<<_Mantissa1:48, 16#F:4, _Mantissa2:4, _Sign:1, 16#7F:7>>) ->
  'NaN';
decode_double(<<Value:64/little-signed-float>>) ->
  Value.

% Parses output data from the sandbox into a supported value.
-spec parse_data(binary()) -> atom() | number() | binary().
parse_data(<<?UNDEFINED>>) ->
  undefined;
parse_data(<<?NULL>>) ->
  null;
parse_data(<<?BOOLEAN, Value:8>>) ->
  case Value of
    0 -> false;
    1 -> true
  end;
parse_data(<<?INT32, Value:32/little-signed-integer>>) ->
  Value;
parse_data(<<?DOUBLE, Value:8/binary>>) ->
  decode_double(Value);
parse_data(<<?STRING, Value/binary>>) ->
  Value.

% Waits for a message to arrive from the sandbox port and unpacks it.
-spec listen(port()) -> {ok | error, atom() | number() | binary()}.
listen(Port) when is_port(Port) ->
  receive
    {Port, {data, Result}} -> parse_result(Result);
    {'EXIT', Port, ExitCode} -> error({js_worker_crash, ExitCode})
  end.

% Packs the arguments for a function call into binary data for the sandbox port.
-spec pack_arguments([atom() | number() | string() | binary()]) -> iodata().
pack_arguments(Arguments) ->
  pack_arguments(Arguments, []).

% Helper for the pack_arguments/1 function.
-spec pack_arguments([atom() | number() | string() | binary()], iodata()) -> iodata().
pack_arguments([], Accumulator) ->
  lists:reverse(Accumulator);
pack_arguments([Current | Remaining], Accumulator) ->
  pack_arguments(Remaining, [pack_argument(Current) | Accumulator]).

% Converts a single argument into binary data for the sandbox port.
-spec pack_argument(atom() | number() | string() | binary()) -> iodata().
pack_argument(undefined) ->
  ?UNDEFINED;
pack_argument(null) ->
  ?NULL;
pack_argument(true) ->
  <<?BOOLEAN, 1>>;
pack_argument(false) ->
  <<?BOOLEAN, 0>>;
pack_argument(Value) when is_integer(Value) ->
  <<?INT32, Value:32/little-signed-integer>>;
pack_argument(Value) when is_float(Value) ->
  <<?DOUBLE, Value:64/little-signed-float>>;
pack_argument(Value) when is_list(Value) ->
  pack_argument(list_to_binary(Value));
pack_argument(Value) when is_binary(Value) ->
  [<<?STRING, (byte_size(Value)):32/little-unsigned-integer>>, Value].
