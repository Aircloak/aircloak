defmodule Cloak.DataSource.ParametersTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Parameters

  test "get parameter when case is off" do
    assert "value" === Parameters.get(%{:kEy => "value"}, "key")
    assert "value" === Parameters.get(%{:kEy => "value"}, "KEY")
    assert "value" === Parameters.get(%{:key => "value"}, "KEY")
    assert "value" === Parameters.get(%{:key => "value"}, :kEY)
  end
end
