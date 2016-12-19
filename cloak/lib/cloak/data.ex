defmodule Cloak.Data do
  @moduledoc "Contains functions for uniformly working with numbers and datetime types."

  @doc "Returns the smaller of the two values according to the order given by `lt_eq`."
  def min(x, y), do: if lt_eq(x, y), do: x, else: y

  @doc "Returns the greater of the two values according to the order given by `lt_eq`."
  def max(x, y), do: if lt_eq(x, y), do: y, else: x

  @doc "Convenience for `not lt_eq`."
  def gt(x, y), do: not lt_eq(x, y)

  @doc """
  Returns true if the first value is less than or equal to the second, false otherwise. Orders `Date` and
  `NaiveDateTime` structs by their order in time. Orders `Time` structs by distance from midnight. Uses the default
  ordering given by `<=` for other values.
  """
  def lt_eq(x = %NaiveDateTime{}, y = %NaiveDateTime{}), do: Timex.diff(x, y) <= 0
  def lt_eq(x = %Date{}, y = %Date{}), do: Timex.diff(x, y) <= 0
  def lt_eq(x = %Time{}, y = %Time{}), do: Cloak.Time.time_to_seconds(x) <= Cloak.Time.time_to_seconds(y)
  def lt_eq(x, y), do: x <= y

  @doc """
  Returns a value that's "infinitesimally" larger than the given value. Should only be used to construct an interval
  of the form `{x, plus_epsilon(x)}`. In those cases when checking the left boundary with `lt_eq` and the right one
  with `gt` only values equal to `x` will fall into the interval (to a good approximation). Produces denormalized `Time`
  structs (like `~T[23:59:60]`), which are nevertheless fine when used in conjunction with other functions from this
  module.
  """
  def plus_epsilon(x = %NaiveDateTime{}), do: Timex.shift(x, seconds: 1)
  def plus_epsilon(x = %Date{}), do: Timex.shift(x, days: 1)
  def plus_epsilon(x = %Time{}), do: %{x | second: x.second + 1}
  def plus_epsilon(x), do: x + x / 100_000_000_000_000
end
