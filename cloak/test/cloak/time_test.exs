defmodule Cloak.Time.Test do
  use ExUnit.Case, async: true

  test "parsing an invalid date", do: assert({:error, :invalid_format} = Cloak.Time.parse_date("invalid"))

  test "1583-01-01 is the minimum parseable date" do
    assert {:ok, ~D[1583-01-01]} = Cloak.Time.parse_date("1583-01-01")
    assert {:error, :pre_gregorian_calendar} = Cloak.Time.parse_date("1582-12-31")
  end

  test "9999-12-31 is the maximum parseable date" do
    assert {:ok, ~D[9999-12-31]} = Cloak.Time.parse_date("9999-12-31")
    assert {:error, :invalid_format} = Cloak.Time.parse_date("10000-01-01")
  end

  test "parsing an invalid datetime", do: assert({:error, :invalid_format} = Cloak.Time.parse_datetime("invalid"))

  test "parsing a full datetime",
    do: assert({:ok, ~N[2000-01-01 01:02:03.000000]} = Cloak.Time.parse_datetime("2000-01-01 01:02:03"))

  test "parsing a date as a datetime",
    do: assert({:ok, ~N[2000-01-01 00:00:00.000000]} = Cloak.Time.parse_datetime("2000-01-01"))

  test "1583-01-01 is the minimum parseable datetime" do
    assert {:ok, ~N[1583-01-01 00:00:00.000000]} = Cloak.Time.parse_datetime("1583-01-01 00:00:00")

    assert {:ok, ~N[1583-01-01 00:00:00.000000]} = Cloak.Time.parse_datetime("1583-01-01")
    assert {:error, :pre_gregorian_calendar} = Cloak.Time.parse_datetime("1582-12-31 23:59:59")
    assert {:error, :pre_gregorian_calendar} = Cloak.Time.parse_datetime("1582-12-31")
  end

  test "9999-12-31 is the maximum parseable datetime" do
    assert {:ok, ~N[9999-12-31 23:59:59.999999]} = Cloak.Time.parse_datetime("9999-12-31 23:59:59.999999")

    assert {:ok, ~N[9999-12-31 00:00:00.000000]} = Cloak.Time.parse_datetime("9999-12-31")
    assert {:error, :invalid_format} = Cloak.Time.parse_datetime("10000-01-01 00:00:00")
    assert {:error, :invalid_format} = Cloak.Time.parse_datetime("10000-01-01")
  end

  test "truncates Logger timestamps to seconds",
    do: assert({{2018, 4, 12}, {9, 7, 9, 0}} = Cloak.Time.truncate({{2018, 4, 12}, {9, 7, 9, 554}}, :second))
end
