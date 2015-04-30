%% @doc Utility module making it easier to access configuration values.
-module(air_conf).

%% API
-export([
  get_val/2
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

-spec get_val(atom(), atom()) -> any().
get_val(SectionName, Name) ->
  do_get_val(SectionName, Name).

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
  case application:get_env(air, Name) of
    {ok, V} -> V;
    _ -> error
  end.
