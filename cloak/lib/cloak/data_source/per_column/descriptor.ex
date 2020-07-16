defmodule Cloak.DataSource.PerColumn.Descriptor do
  @moduledoc """
  Implements a scheme for de-duplicating and indentifying columns across multiple data-sources and cloaks.
  """
  alias Cloak.DataSource
  alias Cloak.DataSource.Table

  # These are connexion parameters that won't affect analysis results.
  @ignored_parameters [:aircloak_udfs, :password]

  @doc """
  Returns a hash suitable for identifying columns across cloaks.
  """
  @spec hash({DataSource.t(), String.t(), String.t()}) :: binary
  def hash({data_source, table_name, column_name}), do: hash(data_source, table_name, column_name)

  @doc """
  Returns a hash suitable for identifying columns across cloaks.
  """
  @spec hash(DataSource.t(), String.t() | Table.t(), String.t()) :: binary
  def hash(data_source, table_or_table_name, column_name) do
    :crypto.hash(
      :sha256,
      :erlang.term_to_binary({
        extract_parameters(data_source),
        extract_anonymizing_info(data_source, table_or_table_name),
        column_name,
        Cloak.Query.Anonymizer.config(:salt)
      })
    )
  end

  defp extract_parameters(data_source) do
    Map.drop(data_source[:parameters] || %{}, @ignored_parameters)
  end

  defp extract_anonymizing_info(data_source, table_name) when is_binary(table_name) do
    table_key = String.to_atom(table_name)

    if Map.has_key?(data_source.tables, table_key) do
      extract_anonymizing_info(data_source, data_source.tables[table_key])
    else
      # In some situations data_source.tables will not have the table_name
      # This happens when creating a new analyst table for example.
      table_name
    end
  end

  defp extract_anonymizing_info(_data_source, table) do
    Map.take(table, [:initial_name, :content_type, :user_id, :user_id_join_chain])
  end
end
