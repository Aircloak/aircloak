%% @doc Helpers for working with date/time data.
-module(time_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([unix_timestamp/0, unix_timestamp/1, unix_timestamp_to_datetime/1]).

% Magical number that is used for converting gregorian seconds to Unix timestamps. It is the result of
% calling calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec unix_timestamp() -> pos_integer().
%% @doc Returns Unix timestamp of the current UTC time.
unix_timestamp() ->
  unix_timestamp(erlang:universaltime()).

-spec unix_timestamp(calendar:datetime()) -> pos_integer().
%% @doc Returns Unix timestamp for the given UTC datetime.
unix_timestamp({{_,_,_},{_,_,_}} = DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

-spec unix_timestamp_to_datetime(pos_integer()) -> calendar:datetime().
%% @doc Returns UTC time for the given Unix timestamp.
unix_timestamp_to_datetime(UnixTime) ->
  calendar:gregorian_seconds_to_datetime(UnixTime + ?UNIX_EPOCH).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

unix_timestamp_test() ->
  ?assertEqual(1391174123, unix_timestamp({{2014, 1, 31}, {13, 15, 23}})),
  ?assertEqual({{2014, 1, 31}, {13, 15, 23}}, unix_timestamp_to_datetime(1391174123)),
  ConvertedNow = {_, {_, _, ConvertedSec}} = unix_timestamp_to_datetime(unix_timestamp()),
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
  ?assertMatch({{Year, Month, Day}, {Hour, Min, _}}, ConvertedNow),
  ?assert(Sec - ConvertedSec =< 1). % One second difference is theoretically possible (though not likely).

-endif.
