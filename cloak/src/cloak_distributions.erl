%% @copyright 2012-2013 Aircloak.
%%
%% @doc Function for generating random variables.
%%      We use and require gaussian distribution.
-module(cloak_distributions).

%% API
-export([
  gauss/4,
  gauss/2,
  gauss_s/3
]).

% This is not exported by the module random, so we have to replicate here.
-type ran() :: {integer(), integer(), integer()}.


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Generates a gaussian distributed random number from
%%      two uniform distributed numbers by the Box-Muller
%%      method.
-spec gauss(integer() | float(), integer() | float(), float(), float()) -> integer().
gauss(Sigma, Mu, Rand1, Rand2) when Rand1 > 0 ->
  R1 = -2.0 * math:log(Rand1),
  R2 = 2.0 * math:pi() * Rand2,
  PreVal = math:sqrt(R1) * math:cos(R2),
  round(Mu + Sigma * PreVal).

%% @doc Generate a gaussian distributed random number with
%%      given standard deviation and mean.
-spec gauss(integer() | float(), integer() | float()) -> integer().
gauss(Sigma, Mu) ->
  case random_util:uniform() of
    0.0 ->
      gauss(Sigma, Mu);
    Rand1 ->
      Rand2 = random_util:uniform(),
      gauss(Sigma, Mu, Rand1, Rand2)
  end.

%% @doc Generate a gaussian distributed random number with
%%      given standard deviation and mean where the random
%%      number is generated with a known seed.
-spec gauss_s(integer() | float(), integer() | float(), ran()) -> integer().
gauss_s(Sigma, Mu, Seed) ->
  case random_util:uniform_s(Seed) of
    {0.0, Seed1} ->
      gauss_s(Sigma, Mu, Seed1);
    {Rand1, Seed1} ->
      {Rand2, _Seed} = random_util:uniform_s(Seed1),
      gauss(Sigma, Mu, Rand1, Rand2)
  end.
