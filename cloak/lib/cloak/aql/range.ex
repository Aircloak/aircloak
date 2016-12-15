defmodule Cloak.Aql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked for ShrinkAndDrop."

  alias Cloak.Aql.{Column, FixAlign}

  @type t :: %__MODULE__{
    column: Column.t,
    interval: FixAlign.interval
  }

  defstruct [column: nil, interval: nil]

  @doc "Returns a Range with the given column and interval."
  def new(column, interval), do: %__MODULE__{column: column, interval: interval}
end
