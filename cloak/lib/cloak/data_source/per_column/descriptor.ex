defmodule Cloak.DataSource.PerColumn.Descriptor do
  @moduledoc """
  Implements a scheme for de-duplicating and indentifying columns across multiple data-sources and cloaks.
  """
  alias Cloak.DataSource

  # These are connexion parameters that won't affect analysis results.
  @ignored_parameters [:aircloak_udfs, :password]

  @doc """
  Returns a hash suitable for identifying columns across cloaks.
  """
  @spec hash({DataSource.t(), String.t(), String.t()}) :: binary
  def hash({data_source, table_name, column_name}), do: hash(data_source, table_name, column_name)
  @spec hash(DataSource.t(), String.t(), String.t()) :: binary
  def hash(data_source, table_name, column_name) do
    :crypto.hash(
      :sha256,
      :erlang.term_to_binary({
        extract_parameters(data_source),
        table_name,
        column_name,
        Cloak.Query.Anonymizer.config(:salt)
      })
    )
  end

  defp extract_parameters(data_source) do
    Map.drop(data_source.parameters, @ignored_parameters)
  end
end
