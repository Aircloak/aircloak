%% @doc The interface point to talk to the air database.
-module(air_db).

%% API
-export([
  call/1
]).

-include("air.hrl").

-type request_fun() :: fun((pgsql_connection:pgsql_connection()) -> any()).
-type request_fun_spec() :: request_fun() | {request_fun(), non_neg_integer() | infinity}.

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Calls the given function on the local cloak database using connection parameters from `app.config'.
-spec call(request_fun_spec()) -> any().
call(Fun) ->
  cloak_db:call(undefined, air_conf:get_val(air_db, connection), Fun).
