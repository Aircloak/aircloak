defmodule Cloak.RNG do
  @moduledoc "Contains functions related to pseudorandom number generation."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  defdelegate gauss(rng), to: :rand, as: :normal_s

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
  Generates a gaussian-distributed random number with the given mean and standard deviation.

    iex> generator = :rand.seed_s(:exsplus, {1, 2, 3})
    iex> {result, _state} = Cloak.RNG.gauss(generator, 10, 3)
    iex> result
    7.132323279497743
  """
  @spec gauss(:rand.state(), number(), number()) :: {float(), :rand.state()}
  def gauss(rng, mean, std_dev) do
    {gauss, rng} = gauss(rng)
    {mean + gauss * std_dev, rng}
  end
end
