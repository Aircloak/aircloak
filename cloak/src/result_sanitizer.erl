%% @doc The result santizer ensures that we do not aggregate
%%      and report more than one answer set per user per task,
%%      and additionally, that all properties reported by a
%%      given user are unique within the result set of that user.
-module(result_sanitizer).

%% API
-export([
  sanitize/1
]).

-include("cloak.hrl").


%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec sanitize([#job_response{}]) -> [#job_response{}].
sanitize(JobResponses) ->
  DupesRemoved = lists:usort(fun compare_by_user/2, JobResponses),
  [JobResponse#job_response{properties=lists:usort(JobResponse#job_response.properties)} ||
    JobResponse <- DupesRemoved].


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

compare_by_user(#job_response{user_id=UserId1}, #job_response{user_id=UserId2}) ->
  UserId1 =< UserId2.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prop() ->
  #property{
    label = "test",
    value = "hello"
  }.

prop_job_response_for(User, Props) ->
  job_response_for(User, Props).

job_response_for(User, Rsp) ->
  #job_response{
    user_id = User,
    properties = Rsp
  }.

filters_out_dupe_users_test() ->
  Resp = prop_job_response_for("sebastian", [prop()]),
  Expected = [#job_response{user_id="sebastian", properties=[prop()]}],
  ?assertEqual(Expected, sanitize([Resp, Resp])).

filters_dupe_props_for_user_test() ->
  Resp = prop_job_response_for("sebastian", [prop(), prop()]),
  Expected = [#job_response{user_id="sebastian", properties=[prop()]}],
  ?assertEqual(Expected, sanitize([Resp])).

-endif.
