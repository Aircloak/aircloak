%% Stub module for cloak_metrics.
%%
%% There's a dependency hell when importing cloak_metrics, so I'm just stubbing
%% it for now.
-module(cloak_metrics).

%% API
-export([
  count/1,
  measure/3,
  histogram/2
]).

count(_) -> ok.
measure(_, _, Fun) -> Fun().
histogram(_, _) -> ok.
