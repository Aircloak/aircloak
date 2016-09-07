defmodule Cloak.DataSource.ParametersTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Parameters

  test "get parameter when case is off" do
    assert "value" === Parameters.get(%{:kEy => "value"}, "key")
    assert "value" === Parameters.get(%{:kEy => "value"}, "KEY")
    assert "value" === Parameters.get(%{:key => "value"}, "KEY")
    assert "value" === Parameters.get(%{:key => "value"}, :kEY)
  end

  test "get parameter value based on one of keys" do
    assert "value" === Parameters.get_one_of(%{:key2 => "value"}, ["key1", "key2"])
  end

  test "returns nil if no match is found" do
    assert nil === Parameters.get_one_of(%{:key => "value"}, ["missing key"])
  end
end
