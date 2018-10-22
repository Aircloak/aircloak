defmodule Aircloak.Test do
  use ExUnit.Case, async: true

  describe "atomize_keys" do
    test "map with string keys" do
      assert %{test: 1} == Aircloak.atomize_keys(%{"test" => 1})
    end

    test "nested maps" do
      assert %{test: %{test: 1}} == Aircloak.atomize_keys(%{"test" => %{"test" => 1}})
    end

    test "mixed maps" do
      assert %{test: %{test: 1}} == Aircloak.atomize_keys(%{test: %{"test" => 1}})
    end
  end
end
