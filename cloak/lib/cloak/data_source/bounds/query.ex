defmodule Cloak.DataSource.Bounds.Query do
  @moduledoc "Implements querying the database to find the bounds of a column."

  alias Cloak.Sql.{Parser, Compiler, Expression}
  alias Cloak.DataSource.{SQLServer, MongoDB, SqlBuilder, Table}
  alias Cloak.DataSource.Bounds.Compute
  alias Cloak.Query.DbEmulator

  @query_limit 1000

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Queries the data source for the bounds of the given column in the given table.

  Always returns either computed bounds or `:unknown` if they cannot be computed (for example because there are too few
  users). Returns `:unknown` for all non-numeric columns.
  """
  @spec bounds(Cloak.DataSource.t(), String.t(), String.t()) :: Expression.bounds()
  def bounds(data_source, table_name, column) do
    table_name = String.to_existing_atom(table_name)
    table = data_source.tables[table_name]

    with false <- overflow_safe?(data_source),
         false <- Table.key?(table, column),
         true <- numeric?(data_source, table_name, column) do
      case table.content_type do
        :public -> public_bounds(data_source, table_name, column)
        _ -> private_bounds(data_source, table_name, column)
      end
    else
      _ -> :unknown
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp public_bounds(data_source, table_name, column) do
    with {:ok, min, max} <- min_max(data_source, table_name, column) do
      Compute.extend({min |> :math.floor() |> round(), max |> :math.ceil() |> round()})
    else
      _ -> :unknown
    end
  end

  defp private_bounds(data_source, table_name, column) do
    with cutoff = cutoff(table_name, column),
         {:ok, max} <- Compute.max(maxes(data_source, table_name, column), cutoff),
         {:ok, min} <- Compute.min(mins(data_source, table_name, column), cutoff) do
      Compute.extend({min, max})
    else
      _ -> :unknown
    end
  end

  defp numeric?(data_source, table_name, column) do
    column =
      data_source.tables[table_name].columns
      |> Enum.find(&(&1.name == column))

    column.type in [:integer, :real]
  end

  defp maxes(data_source, table_name, column), do: extremes(data_source, table_name, column, "DESC", &max/2)

  defp mins(data_source, table_name, column), do: extremes(data_source, table_name, column, "ASC", &min/2)

  defp extremes(data_source, table_name, column, sort_order, comparison_function) do
    {user_id, table_chain} = SqlBuilder.build_table_chain_with_user_id(data_source.tables, table_name)

    """
      SELECT #{user_id}, "#{table_name}"."#{column}"
      FROM #{table_chain}
      WHERE "#{table_name}"."#{column}" IS NOT NULL
      ORDER BY 2 #{sort_order}
      LIMIT #{@query_limit}
    """
    |> run_query(data_source)
    |> Enum.group_by(&hd/1, fn [_user_id, value] -> value end)
    |> Enum.map(fn {_user_id, values} -> Enum.reduce(values, comparison_function) end)
  end

  defp min_max(data_source, table_name, column) do
    """
    SELECT MIN("#{table_name}"."#{column}"), MAX("#{table_name}"."#{column}")
    FROM "#{table_name}"
    """
    |> run_query(data_source)
    |> case do
      [[nil, nil]] -> :error
      [[min, max]] -> {:ok, min, max}
    end
  end

  defp run_query(query, data_source) do
    query
    |> Parser.parse!()
    |> Compiler.compile_direct!(nil, data_source)
    |> DbEmulator.compile()
    |> DbEmulator.select()
  end

  defp cutoff(table_name, column_name) do
    {cutoff, _rng} =
      {table_name, column_name} |> Cloak.RNG.term_to_rng() |> Cloak.RNG.gauss(config(:mean), config(:std_dev))

    cutoff |> round() |> max(config(:min))
  end

  defp overflow_safe?(data_source), do: data_source.driver in [SQLServer, MongoDB]

  defp config(name), do: Application.get_env(:cloak, :bound_size_cutoff) |> Keyword.fetch!(name)
end
