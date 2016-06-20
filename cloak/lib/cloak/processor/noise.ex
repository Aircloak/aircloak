defmodule Cloak.Processor.Noise do
  @moduledoc "Utility module for noisy processing of buckets."

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
  def passes_filter?(count, seed) do
    absolute_lower_bound = config(:absolute_lower_bound)
    soft_lower_bound = config(:soft_lower_bound)
    sigma_soft_lower_bound = config(:sigma_soft_lower_bound)
    set_seed(seed)
    noisy_count = get(sigma_soft_lower_bound, count) |> round()
    count > absolute_lower_bound and noisy_count > soft_lower_bound
  end

  @doc """
  Builds a seed from the given set or map of unique users.

  This function takes either a `MapSet` containing user ids, or a map where
  keys are user ids. Such types ensure that user ids are deduplicated.
  """
  @spec make_seed(MapSet.t | %{String.t => any}) :: seed
  def make_seed(%MapSet{} = users) do
    make_seed_from_unique_users(users)
  end
  def make_seed(%{} = users_map) do
    make_seed_from_unique_users(Map.keys(users_map))
  end

  @doc """
  Seeds the noise generator.

  The following calls to `gauss/2` will return predictable results
  (as long as the `random` module is not used in between calls).
  """
  @spec set_seed(seed) :: :undefined | seed
  def set_seed({a, b, c}), do: :random.seed(a, b, c)

  @doc "Returns the noise configuration value with the specified name."
  @spec config(atom) :: integer
  def config(name), do: :cloak_conf.get_val(:noise, name)

  @doc "Generates a gaussian distributed random integer with given standard deviation and mean."
  @spec get(integer | float, integer | float) :: float
  def get(sigma, mu) do
    case :random.uniform() do
      0.0 ->
        get(sigma, mu)
      rand1 ->
        rand2 = :random.uniform()
        gauss(sigma, mu, rand1, rand2)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp make_seed_from_unique_users(users) do
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

  # Generates a gaussian distributed random number from two
  # uniform distributed numbers by the Box-Muller method.
  defp gauss(sigma, mu, rand1, rand2) when Rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    preval = :math.sqrt(r1) * :math.cos(r2)
    mu + sigma * preval
  end
end
