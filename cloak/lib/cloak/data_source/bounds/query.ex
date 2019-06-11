defmodule Cloak.DataSource.Bounds.Query do
  alias Cloak.Sql.{Parser, Compiler}
  alias Cloak.DataSource.SqlBuilder
  alias Cloak.DataSource.Bounds.Compute
  alias Cloak.Query.DbEmulator

  @query_limit 1000

  def bounds(data_source, table_name, column) do
    table_name = String.to_existing_atom(table_name)

    with true <- numeric?(data_source, table_name, column),
         cutoff = cutoff(table_name, column),
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
      ORDER BY 2 #{sort_order}
      LIMIT #{@query_limit}
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(nil, data_source)
    |> DbEmulator.compile()
    |> DbEmulator.select()
    |> Enum.group_by(&hd/1, &Enum.at(&1, 1))
    |> Enum.map(fn {_user_id, values} -> Enum.reduce(values, comparison_function) end)
  end

  defp cutoff(table_name, column_name) do
    {cutoff, _rng} =
      {table_name, column_name} |> Cloak.RNG.term_to_rng() |> Cloak.RNG.gauss(config(:mean), config(:std_dev))

    cutoff |> round() |> max(config(:min))
  end

  defp config(name), do: Application.get_env(:cloak, :bound_size_cutoff) |> Keyword.fetch!(name)
end
