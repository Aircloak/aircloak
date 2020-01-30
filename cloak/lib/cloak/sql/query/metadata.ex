defmodule Cloak.Sql.Query.Metadata do
  @moduledoc false

  alias Cloak.Sql.{Function, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def metadata(query) do
    %{
      selected_types: selected_types(query.columns),
      parameter_types: Enum.map(Query.parameter_types(query), &stringify/1)
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selected_types(columns), do: columns |> Enum.map(&Function.type/1) |> Enum.map(&stringify/1)

  defp stringify(string) when is_binary(string), do: string
  defp stringify(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp stringify(function) when is_function(function), do: inspect(function)
end
