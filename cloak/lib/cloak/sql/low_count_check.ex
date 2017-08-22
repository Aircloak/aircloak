defmodule Cloak.Sql.LowCountCheck do
  defstruct [:expressions, :type]

  def new(type, expressions), do:
    %__MODULE__{type: type, expressions: expressions}
end
