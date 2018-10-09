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
