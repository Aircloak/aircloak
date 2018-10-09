defmodule DataQuality.Test.Utility.Test do
  use ExUnit.Case

  alias DataQuality.Test.Utility

  @data [
    %{name: "Aircloak", company: false},
    %{name: "Bob", company: false},
    %{name: "Aircloak", company: true}
  ]

  describe ".partition_and_process" do
    test "callback is given all values in partition" do
      assert [2, 1] == Utility.partition_and_process(@data, [:company], fn values, _ -> Enum.count(values) end)
    end

    test "callback is given values of partition parameters" do
      assert ["Aircloak", "Bob"] == Utility.partition_and_process(@data, [:name], fn _, params -> params[:name] end)
    end

    test "can partition by multiple parameters" do
      assert @data ==
               @data
               |> Utility.partition_and_process([:name, :company], fn _, params -> params end)
               |> Enum.sort()
    end
  end
end
