%% @doc Various functions that are used from different HTTP resource handlers.
-module(resource_common).

%% API
-export([
  verify_analyst/1,
  respond_json/2,
  respond_error/2,
  error_response/1,
  path_error/1,
  get_required_headers/2,
  get_header/3,
  get_body/1,
  stop_on_error/3
]).

-include("cloak.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("ej/include/ej.hrl").

-type req_step_result() :: any().
-type req_step_ret_val() :: ok | {ok, req_step_result()} | {halt, pos_integer(), iodata()} | {error, iodata()}.
-type req_step_fun() :: fun(([req_step_result()]) -> [req_step_ret_val()]).
-type req_state() :: any().


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Verifies if the analyst is provided in the header.
-spec verify_analyst(#wm_reqdata{}) -> {ok, analyst()} | {error, binary()}.
verify_analyst(Req) ->
  case wrq:get_req_header("analyst", Req) of
    undefined -> {error, <<"Missing analyst HTTP-HEADER">>};
    AnalystIdStr ->
      try
        {ok, list_to_integer(AnalystIdStr)}
      catch
        error:badarg -> {error, <<"Invalid analyst HTTP-HEADER">>}
      end
  end.

%% @doc Converts the response to JSON and sets the body of the response
-spec respond_json(cloak_util:deep_proplist(), #wm_reqdata{}) -> #wm_reqdata{}.
respond_json(Response, Req) ->
  wrq:set_resp_body(mochijson2:encode(Response), Req).

%% @doc Given the input error string sets the body to appropriate JSON representation.
-spec respond_error(iodata(), #wm_reqdata{}) -> #wm_reqdata{}.
respond_error(Error, Req) ->
  respond_json(error_response(Error), Req).

%% @doc Converts the input error string to the appropriate JSON representation.
-spec error_response(iodata()) -> cloak_util:deep_proplist().
error_response(Error) ->
  [{success, false}, {error, iolist_to_binary(Error)}].

%% @doc Creates the error string for the invalid request.
-spec path_error(#wm_reqdata{}) -> #wm_reqdata{}.
path_error(Req) ->
  respond_error(
        io_lib:format("Invalid request ~s ~s", [cloak_util:stringify(wrq:method(Req)), wrq:path(Req)]),
        Req
      ).

%% @doc Retrieves values of required headers.
-spec get_required_headers([string()], #wm_reqdata{}) -> {error, binary()} | {ok, tuple()}.
get_required_headers(Headers, Req) ->
  HeaderValues = [{Header, wrq:get_req_header(Header, Req)} || Header <- Headers],
  case [Header || {Header, Value} <- HeaderValues, Value == undefined] of
    [] -> {ok, list_to_tuple([Value || {_, Value} <- HeaderValues])};
    MissingHeaders ->
      {error, iolist_to_binary(["missing required headers: ", string:join(MissingHeaders, ", ")])}
  end.

%% @doc Returns the header with the specified name. If the header is not set, the default value is returned.
-spec get_header(string(), #wm_reqdata{}, string()) -> string().
get_header(Name, Req, Default) ->
  case wrq:get_req_header(Name, Req) of
    undefined -> Default;
    Value -> Value
  end.

%% @doc Retrieves the body, and uncompresses it if needed.
-spec get_body(#wm_reqdata{}) -> {ok, binary()} | {error, binary()}.
get_body(Req) ->
  ReqBody = wrq:req_body(Req),
  case wrq:get_req_header("Content-Encoding", Req) of
    "gzip" -> {ok, zlib:gunzip(ReqBody)};
    "identity" -> {ok, ReqBody};
    undefined -> {ok, ReqBody};
    _ -> {error, <<"unknown content encoding">>}
  end.

%% @doc Runs provided functions in a monadic like chain.
%%      Each function receives a list of results from previous ones in chain. The chain is stopped for
%%      a first function that returns an `error' or a `halt' tuple.
-spec stop_on_error(#wm_reqdata{}, req_state(), [req_step_fun()]) -> {{halt, pos_integer()}, #wm_reqdata{}, req_state()}.
stop_on_error(Req, ReqState, Funs) ->
  stop_on_error(Funs, [], Req, ReqState).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

stop_on_error([], [{ReturnCode, Reply}|_], _Req, ReqState) ->
  {{halt, ReturnCode}, Reply, ReqState};
stop_on_error([Fun|Funs], Acc, Req, ReqState) ->
  case Fun(Acc) of
    {halt, Status, Error} ->
      {{halt, Status}, respond_error(Error, Req), ReqState};
    {error, Error} ->
      {{halt, 422}, respond_error(Error, Req), ReqState};
    ok ->
      stop_on_error(Funs, Acc, Req, ReqState);
    {ok, Value} ->
      stop_on_error(Funs, [Value|Acc], Req, ReqState)
  end.
