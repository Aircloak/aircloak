%% @doc The interface point to talk to the air database.
-module(air_db).

%% API
-export([
  call/1,
  db_config/0
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-type request_fun() :: fun((sql_conn:connection()) -> any()).
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
    {host, binary_to_list(air_etcd:get("/settings/air/db/host"))},
    {port, binary_to_integer(air_etcd:get("/settings/air/db/port"))},
    {user, binary_to_list(air_etcd:get("/settings/air/db/username"))},
    {password, binary_to_list(air_etcd:get("/settings/air/db/password"))},
    {database, binary_to_list(air_etcd:get("/settings/air/db/database"))},
    {ssl, binary_to_atom(air_etcd:get("/settings/air/db/ssl"), utf8)}
  ].
