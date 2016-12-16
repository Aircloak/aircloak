defmodule Cloak.Aql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked for ShrinkAndDrop."

  alias Cloak.Aql.{Column, FixAlign}

  @type t :: %__MODULE__{
    column: Column.t,
    interval: FixAlign.interval,
    type: type
  }
  @type type :: :having | :where | :nested_min | :nested_max

  defstruct [:column, :interval, :type]

  @doc "Returns a Range with the given column and interval."
  def new(column, interval, type), do: %__MODULE__{column: column, interval: interval, type: type}
end
