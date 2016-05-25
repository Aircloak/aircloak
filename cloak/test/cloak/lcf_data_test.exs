defmodule Cloak.LCFDataTest do
  use ExUnit.Case, async: true

  alias Cloak.LCFData

  test "produces a list of users associated with a filtered bucket" do
    lcf_data = LCFData.new
    LCFData.add_bucket_users(lcf_data, :prop, [:a, :b])
    LCFData.record_dropped_property(lcf_data, :prop)
    assert Enum.sort(LCFData.filtered_property_counts(lcf_data)) == [{:a, 1}, {:b, 1}]
  end

  test "accounts for a user having multiple buckets filtered" do
    lcf_data = LCFData.new
    LCFData.add_bucket_users(lcf_data, :prop1, [:a])
    LCFData.add_bucket_users(lcf_data, :prop2, [:a])
    LCFData.record_dropped_property(lcf_data, :prop1)
    LCFData.record_dropped_property(lcf_data, :prop2)
    assert LCFData.filtered_property_counts(lcf_data) == [{:a, 2}]
  end
end
