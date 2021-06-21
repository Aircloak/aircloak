defmodule Cloak.DataSource.Bounds.Query do
  @moduledoc "Implements querying the database to find the bounds of a column."

  alias Cloak.Sql.{Parser, Compiler, Expression}
  alias Cloak.DataSource.{SqlBuilder, Table}
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
  def bounds(data_source, table_name, column_name) do
    table_name = String.to_existing_atom(table_name)
    table = data_source.tables[table_name]

    with false <- overflow_safe?(data_source),
         false <- Table.key?(table, column_name),
         boundary_type when boundary_type != nil <- boundary_type(data_source, table_name, column_name) do
      boundary_expression = boundary_expression(boundary_type, table_name, column_name)

      case table.content_type do
        :public -> public_bounds(boundary_type, data_source, table_name, boundary_expression)
        _ -> private_bounds(boundary_type, data_source, table_name, boundary_expression)
      end
    else
      _ -> :unknown
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp public_bounds(type, data_source, table_name, expression) do
    case min_max(data_source, table_name, expression) do
      {:ok, min, max} -> extend(type, {floor(min), ceil(max)})
      _ -> :unknown
    end
  end

  defp private_bounds(type, data_source, table_name, expression) do
    cutoff = cutoff(expression)

    with {:ok, max} <- maxes(data_source, table_name, expression) |> Compute.max(cutoff),
         {:ok, min} <- mins(data_source, table_name, expression) |> Compute.min(cutoff) do
      extend(type, {min, max})
    else
      _ -> :unknown
    end
  end

  defp boundary_type(data_source, table_name, column_name) do
    column =
      data_source.tables[table_name].columns
      |> Enum.find(&(&1.name == column_name))

    cond do
      column.type in [:integer, :real] -> :numeric
      column.type in [:date, :datetime] -> :year
      true -> nil
    end
  end

  # We make the year values relative to lowest valid year, so we get tighter bounds after alignment.
  defp boundary_expression(:year, table_name, column_name),
    do: ~s[YEAR("#{table_name}"."#{column_name}") - #{Cloak.Time.year_lower_bound()}]

  defp boundary_expression(:numeric, table_name, column_name), do: ~s["#{table_name}"."#{column_name}"]

  defp maxes(data_source, table_name, expression), do: extremes(data_source, table_name, expression, "DESC", &max/2)

  defp mins(data_source, table_name, expression), do: extremes(data_source, table_name, expression, "ASC", &min/2)

  defp extremes(data_source, table_name, expression, sort_order, comparison_function) do
    {user_id, table_chain} = SqlBuilder.build_table_chain_with_user_id(data_source.tables, table_name)

    """
      SELECT #{user_id}, #{expression}
      FROM #{table_chain}
      WHERE #{expression} IS NOT NULL
      ORDER BY 2 #{sort_order}
      LIMIT #{@query_limit}
    """
    |> run_query(data_source)
    |> Enum.group_by(&hd/1, fn [_user_id, value] -> value end)
    |> Enum.map(fn {_user_id, values} -> Enum.reduce(values, comparison_function) end)
  end

  defp min_max(data_source, table_name, expression) do
    """
      SELECT MIN(#{expression}), MAX(#{expression}) FROM "#{table_name}"
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

  defp cutoff(expression) do
    {cutoff, _rng} = expression |> Cloak.RNG.term_to_rng() |> Cloak.RNG.gauss(config(:mean), config(:std_dev))

    cutoff |> round() |> max(config(:min))
  end

  defp extend(type, {min, max}) when min > max, do: extend(type, {max, min})

  # For date columns, we extend the interval by 50 years and we make the year bounds absolute.
  defp extend(:year, {min, max}) when max >= min do
    min = Kernel.max(Cloak.Time.year_lower_bound() + min - 25, Cloak.Time.year_lower_bound())
    max = Kernel.min(Cloak.Time.year_lower_bound() + max + 25, Cloak.Time.year_upper_bound())
    {min, max}
  end

  # For numeric columns, we extend the interval by 10x towards infinity or 0.1x towards 0.
  defp extend(:numeric, {min, max}) when max >= min do
    cond do
      min <= 0 and max <= 0 -> {min * 10, div(max, 10)}
      min >= 0 and max >= 0 -> {div(min, 10), max * 10}
      true -> {min * 10, max * 10}
    end
  end

  defp overflow_safe?(_data_source), do: false

  defp config(name), do: Application.get_env(:cloak, :bound_size_cutoff) |> Keyword.fetch!(name)
end
