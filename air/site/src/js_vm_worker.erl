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
  try
    VM = open_sandbox(),
    lists:foreach(fun (JSModule) ->
      {ok, JSContent} = file:read_file(JSModule),
      {ok, _} = evaluate_js(VM, filename:basename(JSModule), JSContent)
    end, JSModules),
    {ok, #state{vm = VM}}
  catch
    error:{js_worker_exit, ExitStatus} ->
      ?ERROR("JS worker crashed during initialization with exit status: ~p", [ExitStatus]),
      {stop, {js_worker_exit, ExitStatus}}
  end.

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
terminate(Reason, _State) ->
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

% Sandbox types ids.
-define(UNDEFINED, 0).
-define(NULL, 1).
-define(BOOLEAN, 2).
-define(INT32, 3).
-define(DOUBLE, 4).
-define(STRING, 5).
-define(ARRAY, 6).
-define(MAP, 7).

% Types of sandbox values.
-type js_type() :: atom() | number() | [js_type()] | binary() | #{js_type() => js_type()}.

-spec open_sandbox() -> port().
open_sandbox() ->
  process_flag(trap_exit, true),
  Binary = code:priv_dir(air) ++ "/js_sandbox/js_sandbox",
  open_port({spawn_executable, Binary}, [{packet, 4}, {args, []}, exit_status, use_stdio, binary]).

-spec close_sandbox(port()) -> ok.
close_sandbox(Port) ->
  port_command(Port, message(?STOP, <<>>, <<>>)),
  receive
    {'EXIT', Port, normal} -> ok
  end.

-spec evaluate_js(port(), string() | binary(), binary()) -> {ok | error, js_type()}.
evaluate_js(Port, ScriptName, ScriptContent) ->
  port_command(Port, message(?EVALUATE, ScriptName, ScriptContent)),
  listen(Port).

-spec call_js(port(), string() | binary(), [js_type()]) -> {ok | error, js_type()}.
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
-spec parse_result(binary()) -> {ok | error, js_type()}.
parse_result(<<Success:8, Data/binary>>) ->
  Status = case Success of
    0 -> error;
    1 -> ok
  end,
  {Value, <<>>} = parse_data(Data),
  {Status, Value}.

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
-spec parse_data(binary()) -> {js_type(), binary()}.
parse_data(<<?UNDEFINED, Rest/binary>>) ->
  {undefined, Rest};
parse_data(<<?NULL, Rest/binary>>) ->
  {null, Rest};
parse_data(<<?BOOLEAN, Value:8, Rest/binary>>) ->
  Atom = case Value of
    0 -> false;
    1 -> true
  end,
  {Atom, Rest};
parse_data(<<?INT32, Value:32/little-signed-integer, Rest/binary>>) ->
  {Value, Rest};
parse_data(<<?DOUBLE, Value:8/binary, Rest/binary>>) ->
  {decode_double(Value), Rest};
parse_data(<<?STRING, Size:32/little-unsigned-integer, Value:Size/binary, Rest/binary>>) ->
  {Value, Rest};
parse_data(<<?ARRAY, Count:32/little-unsigned-integer, Elements/binary>>) ->
  parse_array(Elements, Count, []);
parse_data(<<?MAP, Count:32/little-unsigned-integer, Elements/binary>>) ->
  parse_map(Elements, Count, #{}).

% Unpacks an array from the sandbox.
-spec parse_array(binary(), non_neg_integer(), [js_type()]) -> {[js_type()], binary()}.
parse_array(Data, 0, Array) ->
  {lists:reverse(Array), Data};
parse_array(Data, Count, Array) ->
  {Element, Rest} = parse_data(Data),
  parse_array(Rest, Count - 1, [Element | Array]).

% Unpacks a map from the sandbox.
-spec parse_map(binary(), non_neg_integer(), #{js_type() => js_type()}) -> {#{js_type() => js_type()}, binary()}.
parse_map(Data, 0, Map) ->
  {Map, Data};
parse_map(Data, Count, Map) ->
  {Property, Rest1} = parse_data(Data),
  {Value, Rest2} = parse_data(Rest1),
  parse_map(Rest2, Count - 1, Map#{Property => Value}).

% Waits for a message to arrive from the sandbox port and unpacks it.
-spec listen(port()) -> {ok | error, js_type()}.
listen(Port) when is_port(Port) ->
  receive
    {Port, {data, Result}} -> parse_result(Result);
    {Port, {exit_status, ExitStatus}} -> error({js_worker_exit, ExitStatus});
    {'EXIT', Port, ExitStatus} -> error({js_worker_exit, ExitStatus})
  end.

% Packs the arguments for a function call into binary data for the sandbox port.
-spec pack_arguments([js_type()]) -> iodata().
pack_arguments(Arguments) ->
  pack_arguments(Arguments, []).

% Helper for the pack_arguments/1 function.
-spec pack_arguments([js_type()], iodata()) -> iodata().
pack_arguments([], Accumulator) ->
  lists:reverse(Accumulator);
pack_arguments([Current | Remaining], Accumulator) ->
  pack_arguments(Remaining, [pack_argument(Current) | Accumulator]).

% Converts a single argument into binary data for the sandbox port.
-spec pack_argument(js_type()) -> iodata().
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
pack_argument(Value) when is_atom(Value) ->
  pack_argument(atom_to_binary(Value, utf8));
pack_argument(Value) when is_binary(Value) ->
  [<<?STRING, (byte_size(Value)):32/little-unsigned-integer>>, Value];
pack_argument(Values) when is_list(Values) ->
  PackedList = [pack_argument(Value) || Value <- Values],
  [<<?ARRAY, (length(Values)):32/little-unsigned-integer>>, PackedList];
pack_argument(Values) when is_map(Values) ->
  PackedMap = [[pack_argument(Key), pack_argument(Value)] || {Key, Value} <- maps:to_list(Values)],
  [<<?MAP, (maps:size(Values)):32/little-unsigned-integer>>, PackedMap].
