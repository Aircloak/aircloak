defmodule Cloak.RNG do
  @moduledoc "Contains functions related to pseudorandom number generation."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Seeds an RNG with an arbitrary term.

  Internally, the term is turned into a binary and hashed before extracting the seed for the RNG.

    iex> {result, _} = Cloak.RNG.term_to_rng({:any, :term}) |> :rand.uniform_s()
    iex> result
    0.3970324064245143
  """
  @spec term_to_rng(term()) :: :rand.state()
  def term_to_rng(term) do
    import Bitwise

    binary = :erlang.term_to_binary(term)

    <<a1::16-unsigned, a2::16-unsigned, b1::16-unsigned, b2::16-unsigned, c1::32-unsigned, c2::32-unsigned,
      _::32-unsigned>> = :crypto.hash(:sha, binary)

    :rand.seed_s(:exsplus, {a1 ^^^ a2, b1 ^^^ b2, c1 ^^^ c2})
  end

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

  @doc """
  Generates a gaussian-distributed random number with the given mean and standard deviation.

    iex> generator = :rand.seed_s(:exsplus, {1, 2, 3})
    iex> {result, _state} = Cloak.RNG.gauss(generator, 10, 3)
    iex> result
    6.138498330404623
  """
  @spec gauss(:rand.state(), number(), number()) :: {float(), :rand.state()}
  def gauss(rng, mean, std_dev) do
    {gauss, rng} = gauss(rng)
    {mean + gauss * std_dev, rng}
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
