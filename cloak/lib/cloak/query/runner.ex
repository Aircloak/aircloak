defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.DataSource
  alias Cloak.Query.{Aggregator, NegativeCondition, Sorter}

  defmodule RuntimeError do
    @moduledoc """
    An error that occurred while running the query.

    This error can be used to signal an error that will be caught by the runner
    and reported to the user.

    You're advised to not overuse this mechanism. However, sometimes it can be
    quite complicated to bubble up an error from a deep nested stack of maps,
    reduces, and other transformations. In such cases, you can raise this error
    with a descriptive message which will be reported to the end-user.
    """

    defexception [:message]
  end


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [String.t], [Aggregator.bucket]}} | {:error, any}
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
    rows = DataSource.columns(data_source, table_id)
    |> Enum.map(fn({name, type}) -> %{occurrences: 1, row: [name, type]} end)

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :select, data_source: data_source} = select_query) do
    try do
      with {:ok, {_count, columns, rows}} <- DataSource.select(data_source, select_query) do
        buckets =
          rows
          |> NegativeCondition.apply(columns, select_query)
          |> Aggregator.aggregate(columns, select_query)
          |> Sorter.order(select_query)

        {:ok, {:buckets, Cloak.SqlQuery.column_titles(select_query), buckets}}
      end
    rescue e in [RuntimeError] ->
      {:error, e.message}
    end
  end
end
