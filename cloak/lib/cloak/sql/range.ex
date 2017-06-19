defmodule Cloak.Sql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked."

  alias Cloak.Sql.{Expression, FixAlign}

  @type t :: %__MODULE__{
    column: Expression.t,
    interval: FixAlign.interval(any),
  }

  defstruct [:column, :interval]

  @doc "Returns a Range with the given column and interval."
  @spec new(Expression.t, FixAlign.interval(any)) :: t
  def new(column, interval), do: %__MODULE__{column: column, interval: interval}
end
