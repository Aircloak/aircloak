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
        ok = js_driver:define_js(VM, {file, JSModule})
      end, JSModules),
  {ok, #state{vm = VM}}.

handle_call({call, Func, Args}, _From, #state{vm = VM} = State) ->
  Result = js:call(VM, Func, Args),
  {reply, Result, State};
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
