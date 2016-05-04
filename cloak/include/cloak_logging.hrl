%% Various helpers for simplified logging

-include_lib("aircloak_common/include/elixir_logger.hrl").


%% -------------------------------------------------------------------
%% Metrics related
%% -------------------------------------------------------------------

-define(MEASURE(MetricName, Code), ?MEASURE(MetricName, ms, Code)).
-define(MEASURE(MetricName, Unit, Code), cloak_metrics:measure(MetricName, Unit, fun() -> Code end)).
-define(REPORT_DURATION(MetricName, StartTime),
    cloak_metrics:histogram(MetricName, round(timer:now_diff(os:timestamp(), StartTime) / 1000))).
