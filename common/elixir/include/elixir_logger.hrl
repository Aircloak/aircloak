%% -------------------------------------------------------------------
%% Production logging
%% -------------------------------------------------------------------

-define(LoggerMod, 'Elixir.Aircloak.ErlangLogger').
-define(DefaultMeta, [{file_name, ?FILE}, {line_no, ?LINE}]).

-define(DEBUG(Msg), ?DEBUG(Msg, [])).
-define(DEBUG(Msg, Params), ?DEBUG(Msg, Params, [])).
-define(DEBUG(Msg, Params, Meta), ?LoggerMod:debug(Msg, Params, ?DefaultMeta ++ Meta)).

-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Params), ?INFO(Msg, Params, [])).
-define(INFO(Msg, Params, Meta), ?LoggerMod:info(Msg, Params, ?DefaultMeta ++ Meta)).

-define(WARN(Msg), ?WARN(Msg, [])).
-define(WARN(Msg, Params), ?WARN(Msg, Params, [])).
-define(WARN(Msg, Params, Meta), ?LoggerMod:warn(Msg, Params, ?DefaultMeta ++ Meta)).

-define(ERROR(Msg), ?ERROR(Msg, [])).
-define(ERROR(Msg, Params), ?ERROR(Msg, Params, [])).
-define(ERROR(Msg, Params, Meta), ?LoggerMod:error(Msg, Params, ?DefaultMeta ++ Meta)).
