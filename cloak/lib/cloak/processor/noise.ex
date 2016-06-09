defmodule Cloak.Processor.Noise do
  @moduledoc "Functions for dealing with noised bucket counts."

  @opaque seed :: {integer, integer, integer}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns true if count is sufficiently large to be reported. Sufficiently large means:

  1. Greater than absolute_lower_bound
  2. A noised version of the count is greater than soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec passes_filter?(non_neg_integer, seed) :: boolean
  def passes_filter?(count, random_seed) do
    count > absolute_lower_bound() && noisy_count(count, random_seed) > soft_lower_bound()
  end

  @doc "Alias for passes_filter that can be called from erlang"
  @spec passes_filter(non_neg_integer, seed) :: boolean
  def passes_filter(count, random_seed), do: passes_filter?(count, random_seed)

  @doc """
  Builds a random seed from the given list of users - it's obtained by hashing and will be constant
  for lists containing the same users.
  """
  @spec random_seed([any]) :: seed
  def random_seed(users) do
    users
    |> Enum.sort()
    |> Enum.dedup()
    |> :erlang.term_to_binary()
    |> compute_hash()
    |> binary_to_seed()
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp noisy_count(count, random_seed) do
    :cloak_distributions.gauss_s(sigma_soft_lower_bound(), count, random_seed)
  end

  defp compute_hash(binary), do: :crypto.hash(:md4, binary)

  defp binary_to_seed(binary) do
    <<a::32, b::32, c::64>> = binary
    {a, b, c}
  end

  defp absolute_lower_bound, do: noise_config(:absolute_lower_bound)

  defp soft_lower_bound, do: noise_config(:soft_lower_bound)

  defp sigma_soft_lower_bound, do: noise_config(:sigma_soft_lower_bound)

  defp noise_config(name), do: Application.get_env(:cloak, :noise) |> Keyword.get(name)
end
