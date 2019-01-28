defmodule Cloak.AnalystTable do
  @doc "Service for working with analyst tables"

  use GenServer
  alias Cloak.AnalystTable.Helpers
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(Cloak.Sql.Query.analyst_id(), String.t(), String.t(), DataSource.t()) :: :ok | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
    table = %{analyst: analyst, name: table_name, statement: statement, data_source: data_source}

    with {:ok, query, db_name} <- store_table_to_database(table), do: store_table_to_ets(table, query, db_name)
  end

  @doc "Returns the analyst table definition."
  @spec table_definition!(Cloak.Sql.Query.analyst_id(), String.t(), DataSource.t()) :: DataSource.Table.t()
  def table_definition!(analyst, table_name, data_source),
    do: Enum.find(analyst_tables(analyst, data_source), &(&1.name == table_name))

  @doc "Returns analyst tables for the given analyst in the given data source."
  @spec analyst_tables(Cloak.Sql.Query.analyst_id(), DataSource.t()) :: [DataSource.Table.t()]
  def analyst_tables(analyst, data_source) do
    Enum.map(
      :ets.match(__MODULE__, {{analyst, data_source.name, :_}, :"$1"}),
      fn [table] -> table end
    )
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true, write_concurrency: true])
    {:ok, nil}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp store_table_to_database(table) do
    with {:ok, query} <- Helpers.compile(table.name, table.statement, table.data_source),
         {:ok, db_name} <- Helpers.store({table.analyst, table.name}, query, table.data_source),
         do: {:ok, query, db_name}
  end

  defp store_table_to_ets(table, query, db_name) do
    table_definition =
      table
      |> Map.take([:name, :statement])
      |> Map.put(:db_name, db_name)
      |> table_definition(query)

    :ets.insert(__MODULE__, {{table.analyst, table.data_source.name, table.name}, table_definition})
    :ok
  end

  defp table_definition(table_data, query) do
    DataSource.Table.new(
      table_data.name,
      Cloak.Sql.Compiler.Helpers.id_column(query).name,
      %{db_name: table_data.db_name, columns: columns(query)}
    )
  end

  defp columns(query) do
    Enum.map(
      Enum.zip(query.column_titles, query.columns),
      fn {column_title, column} -> DataSource.Table.column(column_title, column.type) end
    )
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
