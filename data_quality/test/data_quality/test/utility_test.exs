defmodule DataQuality.Test.Utility.Test do
  use ExUnit.Case

  alias DataQuality.Test.Utility

  @data [
    %{name: "Aircloak", company: true},
    %{name: "Aircloak", company: false},
    %{name: "Bob", company: false}
  ]

  describe ".partition" do
    test "returns a single partition in case of no dimensions" do
      assert [@data] == Utility.partition(@data, []) |> Map.values()
    end

    test "records the partition parameter values" do
      assert ["Aircloak", "Bob"] ==
               @data
               |> Utility.partition([:name])
               |> Map.keys()
               |> Enum.map(& &1[:name])
               |> Enum.uniq()
               |> Enum.sort()
    end

    test "each partition contains the relevant records" do
      assert [
               [
                 %{name: "Bob", company: false}
               ],
               [
                 %{name: "Aircloak", company: true},
                 %{name: "Aircloak", company: false}
               ]
             ] ==
               @data
               |> Utility.partition([:name])
               |> Map.values()
               |> Enum.sort()
    end

    test "can partition across multiple partition parameters" do
      assert [
               [
                 %{name: "Aircloak", company: false}
               ],
               [
                 %{name: "Bob", company: false}
               ],
               [
                 %{name: "Aircloak", company: true}
               ]
             ] ==
               @data
               |> Utility.partition([:name, :company])
               |> Map.values()
               |> Enum.sort()
    end
  end

  describe ".maybe_to_number" do
    test "pure text yields identity function" do
      assert "test" == Utility.maybe_to_number("test")
    end

    test "ignores integers with trailing text" do
      assert "123test" == Utility.maybe_to_number("123test")
    end

    test "ignores floats with trailing text" do
      assert "123.00test" == Utility.maybe_to_number("123.00test")
    end

    test "parses integers" do
      assert 123 == Utility.maybe_to_number("123")
    end

    test "parses floats" do
      assert 123.0 == Utility.maybe_to_number("123.00")
    end
  end
end
