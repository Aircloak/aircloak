defmodule Cloak.Sql.NoiseLayer do
  alias Cloak.Sql.Expression

  @type t :: %__MODULE__{name: String.t, expressions: [Expression.t]}

  defstruct [:name, :expressions]

  @doc "Returns a noise layer with the given name, based on the given list of expressions."
  @spec new(String.t, [Expression.t]) :: t
  def new(name, expressions), do: %__MODULE__{name: name, expressions: expressions}
end
