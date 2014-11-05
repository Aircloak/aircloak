%% @doc Handler for websocket connections coming through cowboy.
%% @end

-module(websocket_handler).

%% Cowboy callbacks.
-export([
    init/2,
    websocket_handle/3,
    websocket_info/3,
    terminate/3
  ]).

%% Router callbacks
-export([transmit/2]).

-include("types.hrl").


%% -------------------------------------------------------------------
%% Cowboy callbacks
%% -------------------------------------------------------------------

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

terminate(_Reason, _Req, _State) ->
  router:remove_subscriber(self()),
  ok.

websocket_handle({text, RequestText}, Req, State) ->
  case request:parse(binary_to_list(RequestText)) of
    {subscribe, Path} ->
      router:add_subscriber(#subscriber{id = self(), module = ?MODULE, path = Path}),
      {ok, Req, State};
    _ ->
      {shutdown, Req, State}
  end.

websocket_info({transmit, #article{
      path = Path, content_type = ContentType, content = Content, published_at = PublishedAt
    }}, Req, State) ->
  {MegaSecs, Secs, MicroSecs} = PublishedAt,
  PublishedAtMillis = (MegaSecs * 1000 * 1000 + Secs) * 1000 + MicroSecs div 1000,
  Header = "article path=" ++ Path ++ " content_type=" ++ ContentType ++
    " published_at=" ++ integer_to_list(PublishedAtMillis),
  {reply, [{text, Header}, {binary, Content}], Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.


%% -------------------------------------------------------------------
%% Router callbacks
%% -------------------------------------------------------------------

%% @doc Called to transmit an article to an active subscriber.
%% @end
-spec transmit(#subscriber{}, #article{}) -> ok.
transmit(#subscriber{id = Pid}, Article) ->
  Pid ! {transmit, Article},
  ok.