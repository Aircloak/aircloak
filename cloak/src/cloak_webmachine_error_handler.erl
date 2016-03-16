%% @doc This is a custom error handler
-module(cloak_webmachine_error_handler).

%% web machine callbacks
-export([
  render_error/3
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% In the case of unhnadled exception, webmachine includes stack trace in the
%% response message. Here, we deal with this error ourselves, providing only
%% a generic response error message.
render_error(500, Req, Reason) ->
  {ok, ReqState} = Req:add_response_header("Content-Type", "application/json"),
  ?DEV_ERROR("HTTP 500 Error ~p", [Reason]),
  {erlang:iolist_to_binary("{\"success\": false, \"description\": \"internal server error\"}"), ReqState};
%% For all other cases we delegate to webmachine error handler.
render_error(Code, Req, Reason) ->
  webmachine_error_handler:render_error(Code, Req, Reason).