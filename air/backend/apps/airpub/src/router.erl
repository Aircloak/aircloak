%% @doc The router is the core of the airpub system. It keeps the mapping between a
%%      subscribing process and the subscription path, and it will route newly published articles
%%      towards the interested subscribers.
%%      The router also keeps a short history of previously published articles and it
%%      will send it to a new subscriber, when it registers. This way, a subscriber is
%%      not forced to register before the article is received.
-module(router).

%% API
-export([
  add_subscriber/1,
  remove_subscriber/0,
  publish/1
]).

-include("types.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Registers a new subscriber with the router.
-spec add_subscriber(string()) -> ok.
add_subscriber(Path) ->
  gproc:reg({p, l, {subscriber, Path}}),
  lager:info("Added subscriber: ~p:'~s'~n", [self(), Path]),
  send_history(Path),
  ok.

%% @doc Removes a registered subscriber from the routing tables.
-spec remove_subscriber() -> ok.
remove_subscriber() ->
  lager:info("Removing subscriber ~p~n", [self()]),
  gproc:goodbye(),
  ok.

%% @doc Publish an article to all interested subscribers.
-spec publish(#article{}) -> ok.
publish(Article) ->
  history:append(Article),
  deliver(Article).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Delivers the specified article to the interested subscribers.
-spec deliver(#article{}) -> ok.
deliver(#article{path = Path} = Article) ->
  lager:info("Delivering article at ~s~n", [Path]),
  Nodes = string:tokens(Path, "/"),
  lists:foldl(fun (Node, ParentPath) ->
      CurrentPath = ParentPath ++ "/" ++ Node,
      % Notify all subscribers on all nodes
      rpc:eval_everywhere(gproc, send, [{p, l, {subscriber, CurrentPath}}, {notify, Article}]),
      CurrentPath
    end, "", Nodes),
  ok.

% Send the interesting articles from history to the new subscriber.
-spec send_history(string()) -> ok.
send_history(SubscriberPath) ->
  History = history:filter_by_path(SubscriberPath),
  SortedHistory = lists:sort(fun (A1, A2) ->
      timer:now_diff(A1#article.published_at, A2#article.published_at) < 0
    end, History),
  lists:foreach(fun (Article) ->
      self() ! {notify, Article}
    end, SortedHistory).
