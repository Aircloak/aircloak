%% @doc The router is the core of the airpub system. It keeps the list of subscribers,
%%	mapped by id and subscription path, and it will route newly published articles
%%	towards the interested subscribers.
%%	The router also keeps a short history of previously published articles and it
%%	will send it to a new subscriber, when it registers. This way, a subscriber is
%%	not forced to register before the article is received.
%% @end

-module(router).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/0,
  add_subscriber/1,
  remove_subscriber/1,
  publish/1
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
  id_mappings :: dict:dict(term(), #subscriber{}),
  path_mappings :: dict:dict(string(), #subscriber{}),
  history = [] :: list(#article{}),
  history_size :: integer() % in microseconds
}).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the router as part of a supervisor tree
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, router}, ?MODULE, [], []).

%% @doc Stops the router.
%% @end
-spec stop() -> ok.
stop() ->
  gen_server:call(router, stop).

%% @doc Registers a new subscriber with the router.
%% @end
-spec add_subscriber(#subscriber{}) -> ok.
add_subscriber(Subscriber) ->
  gen_server:call(router, {add_subscriber, Subscriber}).

%% @doc Removes a registered subscriber from the routing tables.
%% @end
-spec remove_subscriber(term()) -> ok.
remove_subscriber(Id) ->
  gen_server:call(router, {remove_subscriber, Id}).

%% @doc Publish an article to all interested subscribers.
%% @end
-spec publish(#article{}) -> ok.
publish(Article) ->
  gen_server:cast(router, {publish, Article}).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, HistorySize} = application:get_env(airpub, publish_history_size),
  State = #state{id_mappings = dict:new(), path_mappings = dict:new(), history_size = HistorySize * 1000 * 1000},
  {ok, State}.

terminate(normal, _State) ->
  ok.

handle_call({add_subscriber, Subscriber}, _From, State) ->
  {reply, ok, add_subscriber(Subscriber, State)};
handle_call({remove_subscriber, Id}, _From, State) ->
  {reply, ok, remove_subscriber(Id, State)};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({publish, Article}, State) ->
  NewState = publish(Article, State),
  {noreply, NewState}.

handle_info(_, State) ->
  io:format("Invalid message received."),
  {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Handler for registering new subscribers.
% It will update the routing tables and send the interesting articles from the history to the new subscriber.
-spec add_subscriber(#subscriber{}, #state{}) -> #state{}.
add_subscriber(
    #subscriber{id = Id, path = Path} = Subscriber,
    #state{
        id_mappings = IdMappings, path_mappings = PathMappings, history = History, history_size = HistorySize
      } = State
    ) ->
  NewIdMappings = dict:append(Id, Subscriber, IdMappings),
  NewPathMappings = dict:append(Path, Subscriber, PathMappings),
  lager:info("Added subscriber: ~p~n", [Subscriber]),
  NewHistory = clean_history(History, HistorySize),
  transmit_history(Subscriber, NewHistory),
  State#state{id_mappings = NewIdMappings, path_mappings = NewPathMappings, history = NewHistory}.

% Handler for unregistering subscribers.
-spec remove_subscriber(term(), #state{}) -> #state{}.
remove_subscriber(Id, #state{id_mappings = IdMappings, path_mappings = PathMappings} = State) ->
  lager:info("Removing subscriber ~p~n", [Id]),
  case dict:find(Id, IdMappings) of
    {ok, Subscribers} ->
      NewIdMappings = dict:erase(Id, IdMappings),
      NewPathMappings = lists:foldl(fun(#subscriber{path = Path} = Subscriber, TempPathMappings) ->
          dict:update(Path, fun(PathSubscribers) ->
              lists:delete(Subscriber, PathSubscribers)
            end, TempPathMappings)
        end, PathMappings, Subscribers),
      State#state{id_mappings = NewIdMappings, path_mappings = NewPathMappings};
    error ->
      State
  end.

% Handler for publishing an article.
-spec publish(#article{}, #state{}) -> #state{}.
publish(#article{path = Path} = Article,
    #state{path_mappings = PathMappings, history = History, history_size = HistorySize} = State) ->
  % clean old articles form history and add the new one
  NewHistory = [Article | clean_history(History, HistorySize)],
  Nodes = string:tokens(Path, "/"),
  publish("", Nodes, PathMappings, Article),
  State#state{history = NewHistory}.

% Send the article to the interested subscriber.
-spec transmit(#subscriber{}, #article{}) -> ok.
transmit(#subscriber{module = Module} = Subscriber, Article) ->
  Module:transmit(Subscriber, Article).

% Does the heavy-lifting for the publish operation by sending the article
% to all interested and registered subscribers.
-spec publish(string(), list(string()), dict:dict(string(), #subscriber{}), #article{}) -> ok.
publish(_Path, [], _PathMappings, _Article) ->
  ok;
publish(ParentPath, [NextNode | RemainingNodes], PathMappings, Article) ->
  CurrentPath = ParentPath ++ "/" ++ NextNode,
  case dict:find(CurrentPath, PathMappings) of
    {ok, Subscribers} ->
      lists:foreach(fun (Subscriber) -> transmit(Subscriber, Article) end, Subscribers);
    error ->
      ok
  end,
  publish(CurrentPath, RemainingNodes, PathMappings, Article).

% Removes old articles from history.
-spec clean_history(list(#article{}), pos_integer())-> list(#article{}).
clean_history(OldHistory, HistorySize) ->
  Now = os:timestamp(),
  [Article || Article <- OldHistory, timer:now_diff(Now, Article#article.published_at) < HistorySize].

% Transmit the interesting articles from history to the new subscriber.
-spec transmit_history(#subscriber{}, list(#article{})) -> ok.
transmit_history(#subscriber{path = SubscriberPath} = Subscriber, History) ->
  lists:foreach(fun (#article{path = ArticlePath} = Article) ->
      IsSubPath = SubscriberPath =:= ArticlePath orelse
        string:substr(ArticlePath, 1, string:len(SubscriberPath) + 1) =:= SubscriberPath ++ "/",
      case IsSubPath of
        true -> transmit(Subscriber, Article);
        false -> ok
      end
    end, History).