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
  filter_by_path/1
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

%% Cleanup interval for old articles.
-define(CLEAN_INTERVAL, 1000). % One second

-record(state, {
  clean_cycle = 1 :: integer(), % incremented on every clean cycle
  history_size :: integer() % in number of clean cycles
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

%% @doc Writes a new article into the history log.
-spec append(#article{}) -> ok.
append(Article) ->
  [{article_clean_cycle, CleanCycle}] = ets:lookup(history, article_clean_cycle),
  ets:insert(history, {CleanCycle, Article#article.path ++ [$/], Article}),
  ok.

%% @doc Returns the articles in the history for the specified path.
-spec filter_by_path(string()) -> [#article{}].
filter_by_path(Path) ->
  lists:flatten(ets:match(history, {'_', Path ++ [$/] ++ '_', '$1'})).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, HistorySize} = application:get_env(airpub, publish_history_size),
  State = #state{clean_cycle = 1, history_size = HistorySize},
  ets:new(history, [bag, named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
  ets:insert(history, {article_clean_cycle, 1 + HistorySize}),
  timer:send_after(?CLEAN_INTERVAL, self(), clean_history),
  {ok, State}.

terminate(normal, _State) ->
  ok.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_, State) ->
  io:format("Invalid cast message received."),
  {noreply, State}.

% Internal message for periodically cleaning up old entries.
handle_info(clean_history, #state{clean_cycle = CleanCycle, history_size = HistorySize} = State) ->
  NewCleanCycle = clean_history(CleanCycle),
  timer:send_after(?CLEAN_INTERVAL, self(), clean_history),
  ets:delete(history, article_clean_cycle),
  ets:insert(history, {article_clean_cycle, NewCleanCycle + HistorySize}),
  {noreply, State#state{clean_cycle = NewCleanCycle}};
handle_info(_, State) ->
  io:format("Invalid info message received."),
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Removes old articles from history.
-spec clean_history(integer())-> integer().
clean_history(CleanCycle) ->
  ets:delete(history, CleanCycle),
  CleanCycle + 1.
