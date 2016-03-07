#!/usr/bin/env escript

%% Takes the deps/cloak app.config and generates the erlang file containing
%% cloak specific configuration. We need to do this, since we're not starting
%% cloak as an OTP application, so we must manually set proper settings.

main(_Args) ->
  {ok, [CloakConf]} = file:consult("deps/cloak/rel/files/app.config"),
  CloakSettings = proplists:get_value(cloak, CloakConf),
  {ok, [AirConf]} = file:consult("rel/files/sys.config"),
  CloakOverrides = proplists:get_value(cloak_overrides, AirConf, []),
  OverriddenCloakSettings = lists:foldl(
        fun({Prop, Val}, Acc) ->
          lists:keystore(Prop, 1, Acc, {Prop, Val})
        end,
        CloakSettings,
        CloakOverrides
      ),
  CloakConfCode = io_lib:format(
        string:join(
              [
                "-module(air_cloak_conf).",
                "-export([config/0]).",
                "",
                "config() ->",
                "  ~p."
              ],
              "\n"
            ),
        [OverriddenCloakSettings]
      ),
  ok = file:write_file("apps/air/src/air_cloak_conf.erl", CloakConfCode).