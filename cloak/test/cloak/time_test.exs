defmodule Cloak.Time.Test do
  use ExUnit.Case, async: true

  test "parsing an invalid date", do:
    assert {:error, :invalid_format} = Cloak.Time.parse_date("invalid")

  test "1583-01-01 is the minimum parseable date" do
    assert {:ok, ~D[1583-01-01]} = Cloak.Time.parse_date("1583-01-01")
    assert {:error, :pre_gregorian_calendar} = Cloak.Time.parse_date("1582-12-31")
  end
end
