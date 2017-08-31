defmodule Cloak.Sql.LowCountCheck do
  @moduledoc "A struct representing a low count check that needs to be performed on the given list of expressions."

  alias Cloak.Sql.Expression

  @type t :: %__MODULE__{expressions: [Expression.t]}

  defstruct [:expressions]

  @doc "Returns a new LowCountCheck with the given list of expressions."
  @spec new([Expression.t]) :: t
  def new(expressions), do:
    %__MODULE__{expressions: expressions}
end
