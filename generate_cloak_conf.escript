#!/usr/bin/env escript

%% Takes the deps/cloak app.config and generates the erlang file containing
%% cloak specific configuration. We need to do this, since we're not starting
%% cloak as an OTP application, so we must manually set proper settings.

main(_Args) ->
  {ok, [CloakConf]} = file:consult("deps/cloak/rel/files/app.config"),
  CloakSettings = proplists:get_value(cloak, CloakConf),
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
        [CloakSettings]
      ),
  ok = file:write_file("src/air_cloak_conf.erl", CloakConfCode).