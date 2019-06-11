defmodule Cloak.RNG do
  @moduledoc "Contains functions related to pseudorandom number generation."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Generates a gaussian-distributed random number.

  Internally, it uses the Box-Muller method.

    iex> generator = :rand.seed_s(:exsplus, {1, 2, 3})
    iex> {result, _state} = Cloak.RNG.gauss(generator)
    iex> result
    -1.287167223198459
  """
  @spec gauss(:rand.state()) :: {float(), :rand.state()}
  def gauss(rng) do
    {rand1, rng} = :rand.uniform_s(rng)
    {rand2, rng} = :rand.uniform_s(rng)
    {gauss(rand1, rand2), rng}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp gauss(rand1, rand2) when rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    :math.sqrt(r1) * :math.cos(r2)
  end
end
