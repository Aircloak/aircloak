%% @doc Utility module making it easier to access configuration
%%      values. The values are read from rel/files/app.config
%%      which, when generating a release, gets compiled into
%%      rel/cloak/etc/app.config
-module(cloak_conf).

%% API
-export([
  get_val/2,
  get_val/3,
  in_development/0
]).

-ifdef(TEST).
-export([
  load_test_config/0
]).
-endif.


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

-ifndef(TEST).
  -spec get_val(atom(), atom()) -> any().
  get_val(SectionName, Name) ->
    do_get_val(SectionName, Name).
-else.
  % While testing, we have to lazily load app environment
  get_val(SectionName, Name) ->
    test_get_val(SectionName, Name).
-endif.

%% @doc Same as get_val/2 if no override is supplied, otherwise return overrider value.
%% @end
-spec get_val(atom(), atom(), undefined | term()) -> any().
get_val(SectionName, Name, undefined) ->
  get_val(SectionName, Name);
get_val(_SectionName, _Name, Override) ->
  Override.

-spec in_development() -> boolean().
in_development() ->
  case get_section(in_development) of
    error ->
      InDevelopment = filelib:is_file("/aircloak/common/config/develop"),
      application:set_env(cloak, in_development, InDevelopment),
      InDevelopment;
    Existing -> Existing
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_get_val(SectionName, Name) ->
  case get_section(SectionName) of
    error -> throw({error, unexpected_missing_section});
    Section ->
      case proplists:get_value(Name, Section, {error, missing}) of
        {error, missing} -> throw({error, {missing_conf_value, Name}});
        Val -> Val
      end
  end.

-spec get_section(atom()) -> any().
get_section(Name) ->
  case application:get_env(cloak, Name) of
    {ok, V} -> V;
    _ -> error
  end.


%% Needed for test purposes. Loads test configuration on first access.
-ifdef(TEST).
  test_get_val(SectionName, Name) ->
    case application:get_env(cloak, config_loaded, false) of
      true -> ok;
      _ -> load_test_config()
    end,
    do_get_val(SectionName, Name).

  load_test_config() ->
    ConfigFile = case os:getenv("PROPER_NODE_APPCONFIG") of
      false -> "test/app.config";
      Otherwise -> Otherwise
    end,
    {ok, WorkingDir} = file:get_cwd(),
    {ok, [ConfigsToLoadForTest]} = case filelib:is_file("../" ++ ConfigFile) of
      true -> file:consult("../" ++ ConfigFile);
      %% When running the proper test, the working directory is the main
      %% cloak-core directory, rather than the .eunit folder used when
      %% running unit tests.
      false -> file:consult(ConfigFile)
    end,
    lists:foreach(fun({ApplicationConfigName, ConfigForApplication}) ->
          lists:foreach(fun({SegmentName, SegmentValue}) ->
                application:set_env(ApplicationConfigName, SegmentName, SegmentValue)
              end, ConfigForApplication)
        end, ConfigsToLoadForTest),
    application:set_env(cloak, config_loaded, true).
-endif.
