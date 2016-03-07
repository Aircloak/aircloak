%% @doc Utility module making it easier to access configuration values.
-module(air_conf).

%% API
-export([
  get_val/2,
  get_section/1
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

  -spec get_section(atom()) -> any().
  get_section(Name) ->
    do_get_section(Name).
-else.
  % While testing, we have to lazily load app environment
  get_val(SectionName, Name) ->
    test_get_val(SectionName, Name).

  get_section(Section) ->
    test_get_section(Section).
-endif.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_get_section(SectionName) ->
  case application:get_env(air, SectionName) of
    {ok, V} -> V;
    _ -> error
  end.

do_get_val(SectionName, Name) ->
  case get_section(SectionName) of
    error -> throw({error, unexpected_missing_section});
    Section ->
      case proplists:get_value(Name, Section, {error, missing}) of
        {error, missing} -> throw({error, {missing_conf_value, Name}});
        Val -> Val
      end
  end.

%% Needed for test purposes. Loads test configuration on first access.
-ifdef(TEST).
  test_get_val(SectionName, Name) ->
    case application:get_env(air, config_loaded, false) of
      true -> ok;
      _ -> load_test_config()
    end,
    do_get_val(SectionName, Name).

  test_get_section(SectionName) ->
    case application:get_env(air, config_loaded, false) of
      true -> ok;
      _ -> load_test_config()
    end,
    do_get_section(SectionName).

  load_test_config() ->
    {ok, [ConfigsToLoadForTest]} = case filelib:is_file("../test/sys.config") of
      true -> file:consult("../test/sys.config")
    end,
    lists:foreach(fun({ApplicationConfigName, ConfigForApplication}) ->
          lists:foreach(fun({SegmentName, SegmentValue}) ->
                application:set_env(ApplicationConfigName, SegmentName, SegmentValue)
              end, ConfigForApplication)
        end, ConfigsToLoadForTest),
    application:set_env(air, config_loaded, true),
    air:load_cloak_config().
-endif.
