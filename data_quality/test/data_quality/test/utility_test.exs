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

  describe ".partition_and_process" do
    test "callback is given all values in partition" do
      assert [2, 1] == Utility.partition_and_process(@data, [:company], fn values, _ -> Enum.count(values) end)
    end

    test "callback is given values of partition parameters" do
      assert ["Aircloak", "Bob"] == Utility.partition_and_process(@data, [:name], fn _, params -> params[:name] end)
    end
  end
end
