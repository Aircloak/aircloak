defmodule Cloak.Test.Generators do
  use ExUnitProperties

  def naive_datetime do
    gen all(date <- date(), time <- time()) do
      {:ok, naive_datetime} = NaiveDateTime.new(date, time)
      naive_datetime
    end
  end

  def date do
    gen all(
          year <- integer(1900..2100),
          month <- integer(1..12),
          day <- integer(1..31),
          result = Date.new(year, month, day),
          match?({:ok, _date}, result)
        ) do
      elem(result, 1)
    end
  end

  def time do
    gen all(hour <- integer(0..23), minute <- integer(0..59), second <- integer(0..59)) do
      Time.from_erl!({hour, minute, second})
    end
  end
end
