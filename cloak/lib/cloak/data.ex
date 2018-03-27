defmodule Cloak.Data do
  @moduledoc "Contains functions for uniformly working with numbers and datetime types."

  @type t :: %NaiveDateTime{} | %Date{} | %Time{} | number | String.t() | nil

  @doc "Returns the smaller of the two values according to the order given by `lt_eq`."
  @spec min(t, t) :: t
  def min(x, y), do: if(lt_eq(x, y), do: x, else: y || x)

  @doc "Returns the greater of the two values according to the order given by `lt_eq`."
  @spec max(t, t) :: t
  def max(x, y), do: if(lt_eq(x, y), do: y, else: x || y)

  @doc "Returns true if the first value is greater than the second, false otherwise. See `lt_eq/2` for details."
  @spec gt(t, t) :: boolean
  def gt(x, y), do: not lt_eq(x, y)

  @doc "Returns true if the first value is greater than the second, false otherwise. See `lt_eq/2` for details."
  @spec lt(t, t) :: boolean
  def lt(x, y), do: gt(y, x)

  @doc """
  Returns true if the first value is greater than or equal to the second, false otherwise. See `lt_eq/2` for details.
  """
  @spec gt_eq(t, t) :: boolean
  def gt_eq(x, y), do: lt_eq(y, x)

  @doc """
  Returns true if the first value is equal to the second, false otherwise. See `lt_eq/2` for details.
  """
  def eq(x, y), do: lt_eq(x, y) and lt_eq(y, x)

  @doc """
  Returns true if the first value is less than or equal to the second, false otherwise. Orders `Date` and
  `NaiveDateTime` structs by their order in time. Orders `Time` structs by distance from midnight. Uses the default
  ordering given by `<=` for other values.
  """
  @spec lt_eq(t, t) :: boolean
  def lt_eq(x = %NaiveDateTime{}, y = %NaiveDateTime{}), do: Timex.diff(x, y) <= 0
  def lt_eq(x = %Date{}, y = %Date{}), do: Timex.diff(x, y) <= 0
  def lt_eq(x = %Time{}, y = %Time{}), do: Cloak.Time.to_integer(x) <= Cloak.Time.to_integer(y)
  def lt_eq(nil, _), do: false
  def lt_eq(_, nil), do: false
  def lt_eq(x, y), do: x <= y

  @doc """
  Returns a value that's "infinitesimally" larger than the given value. Should only be used to construct an interval
  of the form `{x, plus_epsilon(x)}`. In those cases when checking the left boundary with `lt_eq` and the right one
  with `gt` only values equal to `x` will fall into the interval (to a good approximation). Produces denormalized `Time`
  structs (like `~T[23:59:60]`), which are nevertheless fine when used in conjunction with other functions from this
  module.
  """
  @spec plus_epsilon(t) :: t
  def plus_epsilon(x = %NaiveDateTime{}), do: Timex.shift(x, seconds: 1)
  def plus_epsilon(x = %Date{}), do: Timex.shift(x, days: 1)
  def plus_epsilon(x = %Time{}), do: %{x | second: x.second + 1}
  def plus_epsilon(x) when is_number(x), do: x + x / 100_000_000_000_000
end
