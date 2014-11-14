%% @doc Support for parsing and validating text requests from clients.
%%      The request must have the following format: "`<hash> <type> <param1>=<value1> <param2>=<value2> ...'"
%%      The type, parameter names and values must not contain spaces and equal signs.
%%      The hash is the HMAC-SHA2 value of the shared secret and rest of the string (without the following space).
%%      Currently, only "subscribe" requests are supported, which need the "path" and "timestamp" parameters.
%%      The timestamp parameter controls the validity period of the request.
-module(request).

%% API
-export([
  parse/1
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Tries to parse the string into a valid request tuple.
-spec parse(string()) -> invalid | tuple().
parse(Text) ->
  try
    [Hash, Command | Parameters] = string:tokens(Text, " "),
    verify(validate_hash(Hash, string:substr(Text, length(Hash) + 2)), "invalid hash"),
    ParsedParameters = [list_to_tuple(string:tokens(Parameter, "=")) || Parameter <- Parameters],
    validate(Command, lists:keysort(1, ParsedParameters))
  catch
    throw:Message ->
      lager:warning("Failed to validate request '~s' Reason: '~s'~n", [Text, Message]),
      invalid;
    error:Reason ->
      lager:warning("Failed to parse request '~s'. Reason: ~p:~p~n", [Text, Reason, erlang:get_stacktrace()]),
      invalid
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% on failure, throws a message.
-spec verify(boolean(), string()) -> ok.
verify(true, _Message) ->
  ok;
verify(false, Message) ->
  throw(Message).

% Converts a sha256 hash to a string.
-spec sha256_to_string(binary()) -> string().
sha256_to_string(<<Value:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [Value])).

% Validates the HMAC-SHA2 of a string.
-spec validate_hash(string(), string()) -> boolean().
validate_hash(Hash, Text) ->
  {ok, SharedSecret} = application:get_env(airpub, shared_secret),
  Hash == sha256_to_string(crypto:hmac(sha256, SharedSecret, Text)).

% Validates the subscription path.
-spec validate_path(string()) -> boolean().
validate_path([$/ | _Rest]) ->
  true;
validate_path(_Path) ->
  false.

% Validates the timestamp of the request by comparing it with the current time.
-spec validate_timestamp(string()) -> boolean().
validate_timestamp(Timestamp) ->
  RequestSecs = list_to_integer(Timestamp) div 1000,
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  NowSecs = MegaSecs * 1000 * 1000 + Secs,
  {ok, RequestExpirationPeriod} = application:get_env(airpub, request_expiration_period),
  NowSecs - RequestSecs =< RequestExpirationPeriod.

% Validates a request and returns a tuple with the type and parameters if valid.
-spec validate(string(), list(tuple(string(), string()))) -> tuple().
validate("subscribe", [{"path", Path}, {"timestamp", Timestamp}]) ->
  verify(validate_path(Path), "invalid path"),
  verify(validate_timestamp(Timestamp), "invalid timestamp"),
  {subscribe, Path}.
