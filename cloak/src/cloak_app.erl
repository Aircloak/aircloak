%% @doc This module defines the application behaviour of the cloak-core
%%      application. It is the first part of our application
%%      that is run (after dependencies have booted) when the
%%      application is started as a release.
-module(cloak_app).
-behaviour(application).

%% API
-export([
  start/2,
  stop/1,
  add_webmachine_routes/0
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  %% Initialize cluster cron ETS
  cluster_cron:init(),
  %% first ensure that only our error_logger handler is present
  ok = cloak_error_logger_handler:start(),
  %% install alarm handler
  ok = cloak_alarm_handler:install(),
  %% startup master supervisor
  case cloak_sup:start_link() of
    {error, Reason} -> {error, Reason};
    {ok, Pid} ->
      add_webmachine_routes(),
      %% Starts the singleton cloak_metrics server
      cloak_metrics_adapter:start_metrics_server(),
      {ok, Pid}
  end.

add_webmachine_routes() ->
  Routes = [
    %% Interface for executing tasks
    {["task", action], task_resource, []},
    %% Capabilities interface
    {["capabilities"], capabilities_resource, []}
  ],
  [webmachine_router:add_route(Route) || Route <- Routes].

stop(_State) ->
  ok.
