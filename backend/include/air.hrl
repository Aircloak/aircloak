-include_lib("cloak/src/cloak.hrl").
-include("sandbox_pb.hrl").

%% Helper macros for defining supervision tree
-define(SUP(Name), ?SUP(Name, Name, [])).
-define(SUP(SupId, SupModule, StartArgs),
    {SupId, {SupModule, start_link, StartArgs}, permanent, infinity, supervisor, [SupModule]}).
-define(CHILD(I, Type), ?CHILD(I, Type, [])).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
