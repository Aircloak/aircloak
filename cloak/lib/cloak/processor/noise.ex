defmodule Cloak.Processor.Noise do
  @moduledoc "Functions for dealing with noised bucket counts."

  @opaque seed :: {integer, integer, integer}

  import Cloak.Type


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
    count > absolute_lower_bound() and noisy_count(count, random_seed) > soft_lower_bound()
  end

  @doc """
  Builds a random seed from the given set or map of unique users.

  This function takes either a `MapSet` containing user ids, or a map where
  keys are user ids. Such types ensure that user ids are deduplicated.
  """
  @spec random_seed(MapSet.t | %{String.t => any}) :: seed
  def random_seed(%MapSet{} = users) do
    random_seed_from_unique_users(users)
  end
  def random_seed(%{} = users_map) do
    random_seed_from_unique_users(Map.keys(users_map))
  end

  @doc "Returns the noise configuration values with the specified name."
  @spec config(atom) :: integer
  def config(name), do: :cloak_conf.get_val(:noise, name)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp noisy_count(count, random_seed) do
    :cloak_distributions.gauss_s(sigma_soft_lower_bound(), count, random_seed)
  end

  defp random_seed_from_unique_users(users) do
    users
    |> Enum.reduce(compute_hash(""), fn (user, accumulator) ->
      user
      |> to_string()
      |> compute_hash()
      # since the list is not sorted, using `xor` (which is commutative) will get us consistent results
      |> :crypto.exor(accumulator)
    end)
    |> binary_to_seed()
  end

  defp compute_hash(binary), do: :crypto.hash(:md4, binary)

  defp binary_to_seed(binary) do
    <<a::32, b::32, c::64>> = binary
    {a, b, c}
  end

  defp absolute_lower_bound, do: config(:absolute_lower_bound)

  defp soft_lower_bound, do: config(:soft_lower_bound)

  defp sigma_soft_lower_bound, do: config(:sigma_soft_lower_bound)
end
