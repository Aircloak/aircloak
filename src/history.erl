%% @doc The history module keeps a short list of previously published articles that will get sent
%%      to a new subscriber, when it registers. This way, a subscriber is not forced to register
%%      before the article is received.
-module(history).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/0,
  append/1,
  get/0
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

-include("types.hrl").

-record(state, {
  history = [] :: list(#article{}),
  history_size :: integer() % in microseconds
}).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the history process as part of a supervisor tree
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, history}, ?MODULE, [], []).

%% @doc Stops the history process.
-spec stop() -> ok.
stop() ->
  gen_server:call(history, stop).

%% @doc Write a new article into the history log.
-spec append(#article{}) -> ok.
append(Article) ->
  gen_server:cast(history, {append, Article}).

%% @doc Returns the current valid history.
-spec get() -> [#article{}].
get() ->
  gen_server:call(history, get).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, HistorySize} = application:get_env(airpub, publish_history_size),
  State = #state{history_size = HistorySize * 1000 * 1000},
  {ok, State}.

terminate(normal, _State) ->
  ok.

handle_call(get, _From, #state{history = OldHistory, history_size = HistorySize} = State) ->
  NewHistory = clean_history(OldHistory, HistorySize),
  {reply, NewHistory, State#state{history = NewHistory}};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({append, Article}, #state{history = OldHistory, history_size = HistorySize} = State) ->
  NewHistory = clean_history(OldHistory, HistorySize) ++ [Article],
  {noreply, State#state{history = NewHistory}}.

handle_info(_, State) ->
  io:format("Invalid message received."),
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Removes old articles from history.
-spec clean_history(list(#article{}), pos_integer())-> list(#article{}).
clean_history(OldHistory, HistorySize) ->
  Now = os:timestamp(),
  lists:dropwhile(fun (Article) ->
      timer:now_diff(Now, Article#article.published_at) > HistorySize
    end, OldHistory).
