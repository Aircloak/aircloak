%% @doc The interface point to talk to the air database.
-module(air_db).

%% API
-export([
  call/1
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-type request_fun() :: fun((pgsql_connection:pgsql_connection()) -> any()).
-type request_fun_spec() :: request_fun() | {request_fun(), non_neg_integer() | infinity}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Calls the given function on the local cloak database using connection parameters from `app.config'.
-spec call(request_fun_spec()) -> any().
call(Fun) ->
  cloak_db:call(undefined, db_config(), Fun).


db_config() ->
  [
    {host, binary_to_list(air_etcd:get_cached("/settings/rails/db/host"))},
    {user, binary_to_list(air_etcd:get_cached("/settings/rails/db/username"))},
    {password, binary_to_list(air_etcd:get_cached("/settings/rails/db/password"))},
    {database, binary_to_list(air_etcd:get_cached("/settings/rails/db/database"))},
    {port, 5432}
  ].
