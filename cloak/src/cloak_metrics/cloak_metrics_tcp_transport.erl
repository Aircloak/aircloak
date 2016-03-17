%% @doc Tcp transport that can be used to report data via TCP channel.
%% @see cloak_metrics
-module(cloak_metrics_tcp_transport).

%% API
-export([
  init/1, send/2, reconnect/2
]).

%% Types
-type arg() :: {host, string()} | {port, pos_integer()}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec init([arg()]) -> {ok, gen_tcp:socket()} | {error, any()}.
%% @hidden
init(Args) -> connect(Args).

-spec send(iodata(), gen_tcp:socket()) -> ok | {error, closed | inet:posix()}.
%% @hidden
send(Data, Socket) ->
  gen_tcp:send(Socket, Data).

-spec reconnect([arg()], undefined | gen_tcp:socket()) -> {ok, gen_tcp:socket()} | {error, any()}.
%% @hidden
reconnect(Args, undefined) -> connect(Args);
reconnect(Args, Socket) -> gen_tcp:close(Socket), connect(Args).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

connect(Args) ->
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
  gen_tcp:connect(Host, Port,  [{mode, binary}], 5000).
