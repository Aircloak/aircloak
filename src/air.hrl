-include_lib("cloak/src/cloak.hrl").
-include("sandbox_pb.hrl").

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