defmodule Cloak.Processor.AccumulateCountTest do
  use ExUnit.Case, async: true

  require Record
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  alias Cloak.Processor.AccumulateCount

  test "produces a cdf for a single property for a user" do
    user = "user"
    prop = [:a, :b, :c]
    input = [[user | prop], [user | prop], [user | prop]]
    expected = [[user, {prop, 1}], [user, {prop, 2}], [user, {prop, 3}]]
    assert AccumulateCount.pre_process(input) == expected
  end

  test "produces cdfs for multiple properties for a user" do
    user = "user"
    prop1 = [:a, :b, :c]
    prop2 = [:d, :e]
    input = [[user | prop1], [user | prop2]]
    expected = [[user, {prop1, 1}], [user, {prop2, 1}]]
    assert AccumulateCount.pre_process(input) == expected
  end

  test "produces cdfs for multiple users" do
    user1 = "user1"
    user2 = "user2"
    prop1 = [:a, :b, :c]
    prop2 = [:d, :e]
    input = [
      [user1 | prop1], [user1 | prop1], [user1 | prop2],
      [user2 | prop1], [user2 | prop2]
    ]
    expected = [
      [user1, {prop1, 1}], [user1, {prop1, 2}], [user1, {prop2, 1}],
      [user2, {prop1, 1}], [user2, {prop2, 1}]
    ]
    assert AccumulateCount.pre_process(input) == expected
  end

  test "produces accumulate count based on anonymized properties" do
    prop = [:a, :b]
    anonymized_buckets = [
      bucket(property: {prop, 2}, noisy_count: 10),
      bucket(property: {prop, 1}, noisy_count: 10)
    ]
    expected = [bucket(property: prop, noisy_count: 20)]
    assert AccumulateCount.post_process(anonymized_buckets) == expected
  end

  test "produces accumulate count for tailed distributions" do
    prop = [:a, :b]
    anonymized_buckets = [
      bucket(property: {prop, 2}, noisy_count: 5),
      bucket(property: {prop, 1}, noisy_count: 10)
    ]
    expected = [bucket(property: prop, noisy_count: 15)]
    assert AccumulateCount.post_process(anonymized_buckets) == expected
  end

  test "produces reasonable count approximations despite the random noise causing non-monotonically " <>
      "decreasing count values" do
    prop = [:a, :b]
    anonymized_buckets = [
      bucket(property: {prop, 2}, noisy_count: 7),
      bucket(property: {prop, 1}, noisy_count: 5)
    ]
    expected = [bucket(property: prop, noisy_count: 14)]
    assert AccumulateCount.post_process(anonymized_buckets) == expected
  end

  test "order of anonymized properties is irrelevant" do
    prop = [:a, :b]
    anonymized_buckets = [
      bucket(property: {prop, 1}, noisy_count: 10),
      bucket(property: {prop, 2}, noisy_count: 5)
    ]
    expected = [bucket(property: prop, noisy_count: 15)]
    assert AccumulateCount.post_process(anonymized_buckets) == expected
  end

  test "produces accumulate count for distinct properties" do
    prop1 = [:a, :b]
    prop2 = [:c]
    anonymized_buckets = [
      bucket(property: {prop1, 2}, noisy_count: 10),
      bucket(property: {prop1, 1}, noisy_count: 10),
      bucket(property: {prop2, 1}, noisy_count: 5)
    ]
    expected = [
      bucket(property: prop1, noisy_count: 20),
      bucket(property: prop2, noisy_count: 5)
    ]
    assert AccumulateCount.post_process(anonymized_buckets) == expected
  end
end
