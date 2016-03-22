%% Various helpers for simplified logging


%% -------------------------------------------------------------------
%% Production logging
%% -------------------------------------------------------------------

-define(DEBUG(Msg), ?DEBUG(Msg, [])).
-define(DEBUG(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_debug(?FILE, ?LINE, Msg, Params)).

-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_info(?FILE, ?LINE, Msg, Params)).

-define(NOTICE(Msg), ?NOTICE(Msg, [])).
-define(NOTICE(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_notice(?FILE, ?LINE, Msg, Params)).

-define(WARNING(Msg), ?WARNING(Msg, [])).
-define(WARNING(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_warn(?FILE, ?LINE, Msg, Params)).

-define(ERROR(Msg), ?ERROR(Msg, [])).
-define(ERROR(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_error(?FILE, ?LINE, Msg, Params)).

-define(CRITICAL(Msg), ?CRITICAL(Msg, [])).
-define(CRITICAL(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_critical(?FILE, ?LINE, Msg, Params)).

-define(ALERT(Msg), ?ALERT(Msg, [])).
-define(ALERT(Msg, Params), 'Elixir.Cloak.Logger':erlang_log_alert(?FILE, ?LINE, Msg, Params)).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?FILE, ?LINE, ??Var, Var])).


%% -------------------------------------------------------------------
%% Metrics related
%% -------------------------------------------------------------------

-define(MEASURE(MetricName, Code), ?MEASURE(MetricName, ms, Code)).
-define(MEASURE(MetricName, Unit, Code), cloak_metrics:measure(MetricName, Unit, fun() -> Code end)).
-define(REPORT_DURATION(MetricName, StartTime),
    cloak_metrics:histogram(MetricName, round(timer:now_diff(os:timestamp(), StartTime) / 1000))).


%% -------------------------------------------------------------------
%% Development logging
%% -------------------------------------------------------------------

-define(DEV_DEBUG(Msg), ?DEV_DEBUG(Msg, [])).
-define(DEV_DEBUG(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_debug(?FILE, ?LINE, Msg, Params)).

-define(DEV_INFO(Msg), ?DEV_INFO(Msg, [])).
-define(DEV_INFO(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_info(?FILE, ?LINE, Msg, Params)).

-define(DEV_NOTICE(Msg), ?DEV_NOTICE(Msg, [])).
-define(DEV_NOTICE(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_notice(?FILE, ?LINE, Msg, Params)).

-define(DEV_WARNING(Msg), ?DEV_WARNING(Msg, [])).
-define(DEV_WARNING(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_warn(?FILE, ?LINE, Msg, Params)).

-define(DEV_ERROR(Msg), ?DEV_ERROR(Msg, [])).
-define(DEV_ERROR(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_error(?FILE, ?LINE, Msg, Params)).

-define(DEV_CRITICAL(Msg), ?DEV_CRITICAL(Msg, [])).
-define(DEV_CRITICAL(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_critical(?FILE, ?LINE, Msg, Params)).

-define(DEV_ALERT(Msg), ?DEV_ALERT(Msg, [])).
-define(DEV_ALERT(Msg, Params), 'Elixir.Cloak.Logger':erlang_dev_log_alert(?FILE, ?LINE, Msg, Params)).
