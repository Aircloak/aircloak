%% Various helpers for simplified logging

-compile({parse_transform, lager_transform}).


%% -------------------------------------------------------------------
%% Production logging
%% -------------------------------------------------------------------

-define(DEBUG(Msg), lager:debug(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(DEBUG(Msg, Params), lager:debug(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(INFO(Msg), lager:info(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(INFO(Msg, Params), lager:info(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(NOTICE(Msg), lager:notice(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(NOTICE(Msg, Params), lager:notice(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(WARNING(Msg), lager:warning(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(WARNING(Msg, Params), lager:warning(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(ERROR(Msg), lager:error(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(ERROR(Msg, Params), lager:error(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(CRITICAL(Msg), lager:critical(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(CRITICAL(Msg, Params), lager:critical(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(ALERT(Msg), lager:alert(" ~p:~p: ~p", [?MODULE, ?LINE, Msg])).
-define(ALERT(Msg, Params), lager:alert(" ~p:~p: " ++ Msg, [?MODULE, ?LINE | Params])).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).


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

-define(RUN_IN_DEV(Code),
      begin
        case cloak_conf:in_development() of
          false -> ok;
          true -> Code
        end
      end
    ).

-define(DEV_LOG(LogMacro, Msg, Params), ?RUN_IN_DEV(LogMacro(Msg, Params))).
-define(DEV_LOG(LogMacro, Msg), ?RUN_IN_DEV(LogMacro(Msg))).

-define(DEV_DEBUG(Msg), ?DEV_LOG(?DEBUG, Msg)).
-define(DEV_DEBUG(Msg, Params), ?DEV_LOG(?DEBUG, Msg, Params)).

-define(DEV_INFO(Msg), ?DEV_LOG(?INFO, Msg)).
-define(DEV_INFO(Msg, Params), ?DEV_LOG(?INFO, Msg, Params)).

-define(DEV_NOTICE(Msg), ?DEV_LOG(?NOTICE, Msg)).
-define(DEV_NOTICE(Msg, Params), ?DEV_LOG(?NOTICE, Msg, Params)).

-define(DEV_WARNING(Msg), ?DEV_LOG(?WARNING, Msg)).
-define(DEV_WARNING(Msg, Params), ?DEV_LOG(?WARNING, Msg, Params)).

-define(DEV_ERROR(Msg), ?DEV_LOG(?ERROR, Msg)).
-define(DEV_ERROR(Msg, Params), ?DEV_LOG(?ERROR, Msg, Params)).

-define(DEV_CRITICAL(Msg), ?DEV_LOG(?CRITICAL, Msg)).
-define(DEV_CRITICAL(Msg, Params), ?DEV_LOG(?CRITICAL, Msg, Params)).

-define(DEV_ALERT(Msg), ?DEV_LOG(?ALERT, Msg)).
-define(DEV_ALERT(Msg, Params), ?DEV_LOG(?ALERT, Msg, Params)).