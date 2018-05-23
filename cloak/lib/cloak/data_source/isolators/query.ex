defmodule Cloak.DataSource.Isolators.Query do
  alias Cloak.Sql.{Parser, Compiler}

  def isolates_users?(data_source, table, column) do
    isolating_values(data_source, table, column) / (unique_values(data_source, table, column) + 1) > threshold()
  end

  defp isolating_values(data_source, table, column) do
    %{user_id: user_id} = data_source.tables[table]

    """
      SELECT COUNT(*) FROM (
        SELECT #{column}
        FROM #{table}
        GROUP BY #{column}
        HAVING COUNT(#{user_id}) = 1
      ) x
    """
    |> select_count(data_source)
  end

  defp unique_values(data_source, table, column) do
    """
      SELECT COUNT(*) FROM (
        SELECT #{column}
        FROM #{table}
        GROUP BY #{column}
      ) x
    """
    |> select_count(data_source)
  end

  defp select_count(query, data_source) do
    query
    |> Parser.parse!()
    |> Compiler.compile_standard!(data_source)
    |> Cloak.DataSource.select!(&Enum.concat/1)
    |> Enum.count()
  end

  defp threshold(), do: Application.fetch_env!(:cloak, :anonymizer) |> Keyword.fetch!(:isolating_column_threshold)
end
