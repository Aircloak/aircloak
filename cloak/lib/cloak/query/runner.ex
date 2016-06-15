defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.DataSource
  alias Cloak.Query.Result
  alias Cloak.Processor.{NegativeCondition, Anonymizer}

  import Cloak.Type


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [String.t], [Row.t]}} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.make(query.data_source, query.statement) do
      execute_sql_query(sql_query)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp execute_sql_query(%{command: :show, show: :tables, data_source: data_source}) do
    columns = ["name"]
    rows = DataSource.tables(data_source)
    |> Enum.map(fn(table) -> %{occurrences: 1, row: [table]} end)

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :show, show: :columns, from: table_identifier, data_source: data_source}) do
    table_id = String.to_existing_atom(table_identifier)
    columns = ["name", "type"]
    rows = Enum.map(DataSource.columns(data_source, table_id), &Tuple.to_list/1)

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :select, data_source: data_source} = select_query) do
    with {:ok, {_count, [_user_id | columns], rows}} <- DataSource.select(data_source, select_query) do

      buckets = rows
      |> NegativeCondition.apply(columns, select_query)
      |> Result.group_by_property(columns, select_query)
      |> Anonymizer.aggregate(select_query)
      |> Result.map_buckets(select_query)
      |> Result.order_rows(select_query)

      {:ok, {:buckets, Cloak.SqlQuery.column_titles(select_query), buckets}}
    end
  end
end
