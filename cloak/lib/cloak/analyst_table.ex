defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use GenServer
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(Cloak.Sql.Query.analyst_id(), String.t(), String.t(), DataSource.t()) ::
          {:ok, registration_info :: String.t()} | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
    table = %{analyst: analyst, name: table_name, statement: statement, data_source: data_source}

    with {:ok, query} <- Cloak.AnalystTable.Compiler.compile(table.name, table.statement, table.data_source),
         {:ok, db_name, recreate_info} <- store_table_to_database(table, query) do
      table =
        Map.merge(table, %{
          db_name: db_name,
          id_column: Cloak.Sql.Compiler.Helpers.id_column(query).name,
          recreate_info: recreate_info
        })

      store_table_to_ets(table)
      {:ok, registration_info(table)}
    end
  end

  @doc "Registers the analyst table from the given registration info, creating it in the database if needed."
  @spec register_table(String.t()) :: :ok | {:error, String.t()}
  def register_table(registration_info) do
    with {:ok, table} <- table(registration_info),
         :ok <- recreate_table_in_database(table),
         do: store_table_to_ets(table)
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

  defp store_table_to_database(table, query) do
    Cloak.DataSource.Connection.execute!(
      table.data_source,
      &table.data_source.driver.store_analyst_table(&1, {table.analyst, table.name}, query)
    )
  end

  defp recreate_table_in_database(table) do
    DataSource.Connection.execute!(
      table.data_source,
      &table.data_source.driver.recreate_analyst_table(&1, table.db_name, table.recreate_info)
    )
  end

  defp store_table_to_ets(table) do
    table_definition = table_definition(table)
    :ets.insert(__MODULE__, {{table.analyst, table.data_source.name, table.name}, table_definition})
    :ok
  end

  defp table_definition(table) do
    table_definition = DataSource.Table.new(table.name, table.id_column, %{db_name: table.db_name})

    [full_definition] =
      Cloak.DataSource.Connection.execute!(
        table.data_source,
        &table.data_source.driver.load_tables(&1, table_definition)
      )

    full_definition
  end

  defp registration_info(table), do: Poison.encode!(update_in(table.data_source, & &1.name))

  defp table(registration_info) do
    table =
      registration_info
      |> Poison.decode!()
      |> Map.take(~w(analyst name statement data_source db_name id_column recreate_info))
      |> Aircloak.atomize_keys()

    case DataSource.fetch(table.data_source) do
      {:ok, data_source} -> {:ok, %{table | data_source: data_source}}
      :error -> {:error, "Data source not found!"}
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
