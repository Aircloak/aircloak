%% @doc Dispatcher for external calls from lua to Erlang. This module is responsible for whitelisting
%%      provided functions, and delegating to proper implementation.
-module(sandbox_external_call).

%% API
-export([
  call/4
]).

%% Export to trick dialyzer
-ifdef(DIALYZER).
-export([
  dialyzer_use_all_paths/0
]).
-endif.

-include("cloak.hrl").
-include("sandbox_pb.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Handles external lua call.
-spec call(analyst(), user_id(), job_data_streamer:job_input(), #functioncallpb{}) -> #datapb{}.
call(AnalystId, UserId, JobInput, FunctionCall) ->
  % We catch exception in external call to prevent job accidentally (or intentionally) crashing
  % entire job runner.
  Result = try
    do_call(AnalystId, UserId, JobInput, FunctionCall)
  catch _T:_E ->
    undefined
  end,
  encode_result(Result).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_call(_AnalystId, _UserId, JobInput, #functioncallpb{name="get_user_tables", args=#datapbarray{elements=[]}}) ->
  [TableName || {TableName, _} <- JobInput];
do_call(_AnalystId, _UserId, _JobInput, _NotSupported) -> undefined.

encode_result(undefined) -> #datapb{}; % empty datapb corresponds to lua nil
encode_result(Val) when is_number(Val) -> #datapb{double_val=Val};
encode_result(Val) when is_boolean(Val) -> #datapb{bool_val=Val};
encode_result(Val) when is_binary(Val) -> #datapb{string_val=cloak_util:stringify(Val)};
encode_result(Val) when is_list(Val) ->
  #datapb{arr_val=#datapbarray{elements=[encode_result(Element) || Element <- Val]}}.


%% -------------------------------------------------------------------
%% Trick dialyzer
%% -------------------------------------------------------------------

%% Dialyzer is picky about encode_result/1 as not all paths are used.
%% In fact it is only called for `undefined' and `binary()' parameters.
%% This here is there to trick dialyzer into thinking that we use all
%% kinds of values.  It should be removed as soon as we add corresponding
%% external calls that use the full implementation of encode_result/1.

-ifdef(DIALYZER).
dialyzer_use_all_paths() ->
  encode_result([1, true, 1.0]).
-endif.
