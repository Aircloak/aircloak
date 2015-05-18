%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-behaviour(gen_server).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  start_link/0,
  get/1,
  get_cached/1
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

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(string() | binary()) -> binary().
get(Key) ->
  {ok, #get{value=Value}} = etcd:get(etcd_url(), Key, 5000),
  Value.

%% @doc Retrieves the cached value under given key. If the value is not present
%%      in the cache, it is fetched from etcd and cached. Raises an error if the
%%      value under given key is not present.
-spec get_cached(string() | binary()) -> binary().
get_cached(Key) ->
  case cached_value(Key) of
    undefined ->
      {ok, Value} = gen_server:call(?MODULE, {get_cached, Key}),
      Value;
    Cached -> Cached
  end.


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  ets:new(?MODULE, [set, protected, named_table, {read_concurrency, true}]),
  {ok, undefined}.

%% @hidden
handle_call({get_cached, Key}, _, State) ->
  {reply, get_from_cache_or_etcd(Key), State};
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-ifdef(TEST).
  etcd_url() -> "http://127.0.0.1:4003".
-else.
  etcd_url() ->
    lists:flatten(io_lib:format("http://~s:~s", [env("ETCD_HOST", "127.0.0.1"), env("ETCD_PORT", "4002")])).

  env(VarName, Default) ->
    case os:getenv(VarName) of
      false -> Default;
      Value -> Value
    end.
-endif.

cached_value(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{Key, Value}] -> Value;
    _ -> undefined
  end.

get_from_cache_or_etcd(Key) ->
  try
    case cached_value(Key) of
      undefined ->
        V = get(Key),
        ets:insert(?MODULE, {Key, V}),
        {ok, V};
      Cached -> {ok, Cached}
    end
  catch T:E ->
    ?ERROR("Error fetching etcd value ~p at ~p", [{T, E}, erlang:get_stacktrace()]),
    {error, E}
  end.
