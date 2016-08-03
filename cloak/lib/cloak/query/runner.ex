defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.SqlQuery
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
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [String.t], [Aggregator.bucket]}, [String.t]} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.make(query.data_source, query.statement) do
      execute_sql_query(sql_query)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp execute_sql_query(%SqlQuery{command: :show, show: :tables} = query) do
    columns = ["name"]
    rows = DataSource.tables(query.data_source)
    |> Enum.map(fn(table) -> %{occurrences: 1, row: [table]} end)

    successful_result({:buckets, columns, rows}, query)
  end
  defp execute_sql_query(%SqlQuery{command: :show, show: :columns} = query) do
    columns = ["name", "type"]
    [table] = query.selected_tables
    rows = Enum.map(
      table.columns,
      fn({name, type}) -> %{occurrences: 1, row: [name, type]} end
    )

    successful_result({:buckets, columns, rows}, query)
  end
  defp execute_sql_query(%SqlQuery{command: :select} = query) do
    try do
      with {:ok, rows} <- DataSource.select(query) do
        buckets =
          rows
          |> NegativeCondition.apply(query)
          |> Aggregator.aggregate(query)
          |> Sorter.order(query)

        successful_result({:buckets, query.column_titles, buckets}, query)
      end
    rescue e in [RuntimeError] ->
      {:error, e.message}
    end
  end

  defp successful_result(result, query),
    do: {:ok, result, Enum.reverse(query.info)}
end
