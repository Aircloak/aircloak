defmodule Cloak.Processor.LowCountFilterTest do
  use ExUnit.Case, async: true

  require Record
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  alias Cloak.Processor.LowCountFilter

  test "should leave non-lcf buckets be" do
    anonymized_buckets = [
      bucket(property: "property", noisy_count: 5),
    ]
    expected = {[], [bucket(property: "property", noisy_count: 5)]}
    assert LowCountFilter.process_lcf(anonymized_buckets, [1,2,3]) == expected
  end

  test "it should update the lcf property to include as many columns as the result" do
    lcf_bucket = bucket(property: ["aircloak_lcf_tail"], noisy_count: 5)
    anonymized_buckets = [lcf_bucket]
    expected = {[bucket(lcf_bucket, property: ["*", "*", "*"])], []}
    assert LowCountFilter.process_lcf(anonymized_buckets, [1,2,3]) == expected
  end
end
