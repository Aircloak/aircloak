defmodule Cloak.Processor.LowCountFilter do
  @moduledoc """
  Produces an appropriate low-count-filter value based on the number of queried rows.
  For example, a query that has selected three rows, needs a low count filter response
  that also contains three rows.
  """

  require Record
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  @type bucket :: record(:bucket, property: [any], noisy_count: pos_integer)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Given a list of anonymized buckets, it will filter out the low count filter
  bucket and leave the other anonymized buckets as they are. The low count
  filter bucket is updated to one that makes semantic sense given the number
  of rows that were queried
  """
  @spec process_lcf([bucket], [any]) :: {[bucket], [bucket]}
  def process_lcf(anonymized_buckets, columns) do
    lcf_filter = fn(b) -> bucket(b, :property) == ["aircloak_lcf_tail"] end
    {lcf_buckets, other_buckets} = Enum.partition(anonymized_buckets, lcf_filter)
    lcf_property = List.duplicate("*", length(columns))
    lcf_buckets = Enum.map(lcf_buckets, fn(lcf_bucket) -> bucket(lcf_bucket, property: lcf_property) end)
    {lcf_buckets, other_buckets}
  end
end
