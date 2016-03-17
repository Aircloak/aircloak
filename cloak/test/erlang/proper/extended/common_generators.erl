%% @doc This module contains common generators for PropEr.
%% @end
-module(common_generators).

-include("deps/proper/include/proper.hrl").
-include("src/cloak.hrl").

-export([
  subset_generic/4,
  subset/3
]).


%% -------------------------------------------------------------------
%% Subset generator
%% -------------------------------------------------------------------

subset_generic(Generator, List, FreqYes, FreqNo) ->
  subset_generic([], Generator, List, FreqYes, FreqNo).

subset_generic(Acc, _Generator, [], _FreqYes, _FreqNo) ->
  return(Acc);
subset_generic(Acc, Generator, [Item1|Rest], FreqYes, FreqNo) ->
  frequency([
    {FreqNo, subset_generic(Acc, Generator, Rest, FreqYes, FreqNo)},
    {FreqYes, ?LET(Item2, Generator(Item1), subset_generic([Item2|Acc], Generator, Rest, FreqYes, FreqNo))}
  ]).

subset(List, FreqYes, FreqNo) ->
  subset_generic(fun proper_types:return/1, List, FreqYes, FreqNo).
