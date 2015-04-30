-include_lib("cloak/src/cloak.hrl").
-include("sandbox_pb.hrl").

%% Helper macros for defining supervision tree
-define(SUP(Name), ?SUP(Name, Name, [])).
-define(SUP(SupId, SupModule, StartArgs),
    {SupId, {SupModule, start_link, StartArgs}, permanent, infinity, supervisor, [SupModule]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-ifdef(TEST).

-define(LOAD_CONFIG, (fun() ->
      case application:get_env(air, config_loaded, false) of
        false ->
          {ok, [ConfigsToLoadForTest]} = file:consult("../test/sys.config"),
          lists:foreach(fun({ApplicationConfigName, ConfigForApplication}) ->
                lists:foreach(fun({SegmentName, SegmentValue}) ->
                      application:set_env(ApplicationConfigName, SegmentName, SegmentValue)
                    end, ConfigForApplication)
              end, ConfigsToLoadForTest),
          application:set_env(air, config_loaded, true),
          air:load_cloak_config();
        true -> ok
      end
    end)()).

-endif.