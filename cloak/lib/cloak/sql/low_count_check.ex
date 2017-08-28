defmodule Cloak.Sql.LowCountCheck do
  defstruct [:expressions]

  def new(expressions), do:
    %__MODULE__{expressions: expressions}
end
