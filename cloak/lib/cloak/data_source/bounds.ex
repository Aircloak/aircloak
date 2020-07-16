defmodule Cloak.DataSource.Bounds do
  @moduledoc "Entry point for checking column bounds."

  require Aircloak
  require Cloak.Sql.Expression

  alias Cloak.Sql.Expression

  @cache_module __MODULE__.Cache

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the bounds of the given column."
  @spec bounds(Cloak.DataSource.t(), String.t() | Cloak.DataSource.Table.t(), Expression.t()) ::
          Expression.bounds()
  def bounds(%{bound_computation_enabled: true} = data_source, table, %Expression{type: type} = column)
      when type in [:integer, :real, :date, :datetime],
      do: cache_value(data_source, table, column.name)

  def bounds(_data_source, _table, _column), do: :unknown

  defdelegate cache_lookup(data_source, table_name, column_name), to: @cache_module, as: :lookup
  defdelegate cache_value(data_source, table_name, column_name), to: @cache_module, as: :value

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(arg) do
    Aircloak.ChildSpec.supervisor(
      [
        # The cache table is owned by a separate process. This is mostly done for testing purposes, but it also improves
        # fault-tolerance. If the cache process crashes, the cache table will survive.
        __MODULE__.PersistentKeyValue,
        {@cache_module, arg}
      ],
      strategy: :rest_for_one
    )
  end
end
