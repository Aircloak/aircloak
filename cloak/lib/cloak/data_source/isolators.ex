defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestIsolatorsCache, else: Cloak.DataSource.Isolators.Cache)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    case preconfigured(data_source, table, column) do
      {:ok, result} -> result
      :error -> @cache_module.isolates_users?(data_source, table, column)
    end
  end

  @doc "Performs a cache lookup for the given column."
  @spec cache_lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, boolean} | {:error, :failed | :pending | :unknown_column}
  def cache_lookup(data_source, table_name, column_name) do
    case preconfigured(data_source, table_name, column_name) do
      {:ok, result} -> {:ok, result}
      :error -> @cache_module.lookup(data_source, table_name, column_name)
    end
  end

  @doc "Returns unspecified columns that default to isolating due to the table configuration"
  @spec unspecified_columns(Cloak.DataSource.t()) :: %{String.t() => [String.t()]}
  def unspecified_columns(data_source) do
    data_source[:tables]
    |> Enum.filter(&only_manually_classified_tables/1)
    |> Enum.map(&unspecified_columns_from_table/1)
    |> Enum.filter(&tables_with_columns/1)
    |> Enum.into(%{})
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp preconfigured(data_source, table_name, column) do
    {_, table} = Enum.find(data_source.tables, fn {_key, table} -> table.name == table_name end)

    if table.auto_isolating_column_classification do
      Map.fetch(table.isolating_columns, column)
    else
      {:ok, Map.get(table.isolating_columns, column, true)}
    end
  end

  defp only_manually_classified_tables({_name, %{auto_isolating_column_classification: state}}), do: not state

  defp tables_with_columns({_name, columns}), do: length(columns) > 0

  defp unspecified_columns_from_table({name, table}) do
    manually_classified_columns = table[:isolating_columns] || %{}

    unspecified_column_names =
      table[:columns]
      |> Enum.map(& &1.name)
      |> Enum.filter(&(not Map.has_key?(manually_classified_columns, &1)))

    {name, unspecified_column_names}
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(arg) do
    Aircloak.ChildSpec.supervisor(
      [
        # The cache table is owned by a separate process. This is mostly done for testing purposes, but it also improves
        # fault-tolerance. If the cache process crashes, the cache table will survive.
        __MODULE__.CacheOwner,
        {@cache_module, arg}
      ],
      strategy: :rest_for_one
    )
  end
end
