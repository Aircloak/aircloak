%% @doc Represents a worker JavaScript virtual machine, implemented as a
%%      gen_server wrapper over a js_driver context, and which is part
%%      of a pool managed by the js_vm_sup module that can be used
%%      to execute JavaScript code.
-module(js_vm_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% poolboy_worker callbacks
-export([
  start_link/1
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


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(JSModules) when is_list(JSModules) ->
  {ok, VM} = js_driver:new(),
  lists:foreach(fun (JSModule) ->
        ok = js_driver:define_js(VM, {file, JSModule}, infinity)
      end, JSModules),
  {ok, #state{vm = VM}}.

handle_call({call, Func, Args}, _From, #state{vm = VM} = State) ->
  {reply, js_call(VM, Func, Args), State};
handle_call(Message, _From, State) ->
  lager:error("unknown call to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

handle_cast(Message, State) ->
  lager:error("unknown cast to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

handle_info(Message, State) ->
  lager:error("unknown info to js_vm_worker: ~p", [Message]),
  {noreply, State, infinity}.

terminate(_Reason, #state{vm = VM}) ->
  js_driver:destroy(VM),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Call a function by name with a list of arguments.
-spec js_call(port(), binary(), list(any())) -> {ok, any()} | {error, any()}.
js_call(VM, FunctionName, Args) ->
    ArgList = build_arg_list(Args, []),
    EscapedFunctionName = binary:replace(FunctionName, <<"\"">>, <<"\\\"">>, [global]),
    JS = iolist_to_binary([<<"function() { if (">>, FunctionName, <<" === undefined) { throw(\"">>,
          EscapedFunctionName, <<" not defined\"); } ">>, <<"return ">>,
          FunctionName, <<"(">>, ArgList, <<");">>, <<"}();">>]),
    js_driver:eval_js(VM, JS, infinity).

% Converts arguments to JSON.
-spec build_arg_list([any()], [any()]) -> iolist().
build_arg_list([], Accum) ->
    lists:reverse(Accum);
build_arg_list([H|[]], Accum) ->
    build_arg_list([], [jiffy:encode(H)|Accum]);
build_arg_list([H|T], Accum) ->
    build_arg_list(T, [[jiffy:encode(H), ","]|Accum]).
