defmodule Cloak.AggregatorTest do
  use ExUnit.Case, async: true

  alias Cloak.Aggregator
  use Cloak.Type

  test "properly aggregates properties" do
    aggregator = Aggregator.new()
    Aggregator.add_property(aggregator, "user_1", "prop_1")
    Aggregator.add_property(aggregator, "user_1", "prop_2")
    Aggregator.add_property(aggregator, "user_1", "prop_1")
    Aggregator.add_property(aggregator, "user_2", "prop_1")
    Aggregator.add_property(aggregator, "user_2", "prop_3")
    Aggregator.add_property(aggregator, "user_2", "prop_3")
    properties = for item <- Aggregator.gather_buckets(aggregator) do
      {bucket(item, :property), bucket(item, :count)}
    end
    assert Enum.sort(properties) == [{"prop_1", 2}, {"prop_2", 1}, {"prop_3", 1}]
  end
end
