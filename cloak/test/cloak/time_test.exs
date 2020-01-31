defmodule Cloak.Time.Test do
  use ExUnit.Case, async: true

  test "parsing an invalid date", do: assert({:error, :invalid_format} = Cloak.Time.parse_date("invalid"))

  test "parsing an invalid datetime", do: assert({:error, :invalid_format} = Cloak.Time.parse_datetime("invalid"))

  test "parsing a full datetime",
    do: assert({:ok, ~N[2000-01-01 01:02:03.000000]} = Cloak.Time.parse_datetime("2000-01-01 01:02:03"))

  test "parsing a date as a datetime",
    do: assert({:ok, ~N[2000-01-01 00:00:00.000000]} = Cloak.Time.parse_datetime("2000-01-01"))
end
