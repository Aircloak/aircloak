defmodule Cloak.Aggregator do
  @moduledoc """
  Aggregates a set of properties into buckets.

  Here, we collect all properties, group them together, and remove duplicates for individual users
  (multiple responses per distinct property-user combination).

  Each bucket also contains a hash (md4) of a distinct list of users that reported the particular bucket,
  which is necessary for later anonymization steps.

  Internally, aggregator relies on an ETS table. It has been experimentally established that this approach
  performs much better than the purely functional approach relying on dicts and sets.
  """

  use Cloak.Type

  @type t :: :ets.tab


  ## -----------------------------------------------------------------
  ## API functions
  ## -----------------------------------------------------------------

  @doc "Creates a new aggregator."
  @spec new() :: t
  def new() do
    # The aggregator is internally an ETS bag table, with properties as keys and users as values.
    # Furthermore, the table is public, and concurrency options are set.
    # This allows us to quickly insert/deduplicate when aggregator is used by multiple processes.
    # In these cases, we want to quickly insert data and thus allow process to handle subsequent
    # requests as soon as possible. In particular, we get then following properties:
    #
    #   1. Insertion of properties is very fast.
    #   2. Different processes can insert properties in parallel.
    #   3. Properties are immediately deduplicated during insertion.
    #   4. There is no aggregator process, which means no single process bottleneck,
    #      no large mailbox queues, and no scheduler switching overhead.
    #
    # The final aggregation takes place when buckets are finally requested.
    :ets.new(:aggregator, [:bag, :public, {:write_concurrency, true}, {:read_concurrency, false}])
  end

  @doc "Releases the memory occupied by the aggregator."
  @spec delete(t) :: :true
  def delete(aggregator) do
    :ets.delete(aggregator)
  end

  @doc "Adds a property for the given user to the aggregator."
  @spec add_property(t, User.t, Property.t) :: :true
  def add_property(aggregator, user, property) do
    :ets.insert(aggregator, {property, user})
  end

  @doc """
  Aggregates the properties into buckets.

  If {@link Cloak.LCFData} is provided, it will be used to collect candidates for low count filter (LCF) tail.
  """
  @spec gather_buckets(t, Cloak.LCFData.t | :undefined) :: [Bucket.t]
  def gather_buckets(aggregator, lcf_data \\ :undefined) do
    gather_loop(aggregator, :ets.first(aggregator), [], lcf_data)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp gather_loop(_aggregator, :'$end_of_table', buckets, _lcf_data), do: buckets
  defp gather_loop(aggregator, property, buckets, lcf_data) do
    users = (for {^property, user} <- :ets.lookup(aggregator, property), do: user) |> Enum.sort()
    hash = users |> Enum.reduce(:crypto.hash(:md4, ""), fn (user, list_hash) ->
      user_hash = :crypto.hash(:md4, to_string(user))
      :crypto.exor(user_hash, list_hash)
    end)
    count = length(users)
    gather_lcf_users(lcf_data, property, count, users)
    bucket = bucket(property: property, count: count, noisy_count: count, users_hash: hash)
    gather_loop(aggregator, :ets.next(aggregator, property), [bucket | buckets], lcf_data)
  end

  # The absolute count above which we consider that buckets will not be LCF-ed.
  # Since LCF is not deterministic, we reject all buckets where count > threshold + 4 sigma, since it is
  # highly unlikely that such buckets will be rejected. This leaves a possibility that we won't have some
  # users for rejected buckets, but this will only skew the reported LCF tail count, and not compromise
  # the anonymization.
  @threshold :cloak_conf.get_val(:noise, :soft_lower_bound)
  @sigma :cloak_conf.get_val(:noise, :sigma_soft_lower_bound)
  @lcf_certainty_threshold  @threshold + 4 * @sigma
  defp gather_lcf_users(:undefined, _property, _count, _users), do: :ok
  defp gather_lcf_users(_lcf_data, _property, count, _users) when count >= @lcf_certainty_threshold, do: :ok
  defp gather_lcf_users(lcf_data, property, _count, users) do
    Cloak.LCFData.add_bucket_users(lcf_data, property, users)
  end
end
