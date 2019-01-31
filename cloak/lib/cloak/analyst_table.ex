defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(Cloak.Sql.Query.analyst_id(), String.t(), String.t(), DataSource.t()) ::
          {:ok, registration_info :: String.t()} | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
    # Using global ensures that drop_unused_analyst_tables can't run while the store is running. On the other hand,
    # multiple stores can run simultaneously since the lock id (`:store`) is not pid-dependent.
    :global.trans(
      {:database_store, :store},
      fn ->
        table = %{analyst: analyst, name: table_name, statement: statement, data_source: data_source}

        with {:ok, query} <- Cloak.AnalystTable.Compiler.compile(table.name, table.statement, table.data_source),
             {db_name, store_info} = data_source.driver.prepare_analyst_table({analyst, table_name}, query),
             id_column = Cloak.Sql.Compiler.Helpers.id_column(query).name,
             table = Map.merge(table, %{db_name: db_name, id_column: id_column, store_info: store_info}),
             :ok <- store_table_in_database(table) do
          store_table_to_ets(table)
          {:ok, registration_info(table)}
        end
      end,
      [node()]
    )
  end

  @doc "Registers the analyst tables from the given registration infos, creating them in the database if needed."
  @spec register_table(String.t()) :: :ok
  def register_tables(registration_infos) do
    Enum.each(registration_infos, &register_table/1)
    drop_unused_analyst_tables()
    :ok
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

  defp store_table_in_database(table) do
    DataSource.Connection.execute!(
      table.data_source,
      &table.data_source.driver.store_analyst_table(&1, table.db_name, table.store_info)
    )
  end

  defp drop_unused_analyst_tables() do
    # Using global ensures that this function is running in isolation to all other database operations (including
    # concurrent executions of this function.
    :global.trans(
      {:database_store, {:drop_unused_analyst_tables, self()}},
      fn ->
        # We're taking a list of all known registered db_names in all the data sources, since one name can be valid
        # in one data source, but unknown in another which uses the same database. Taking all the names ensures that
        # such tables are not removed.
        db_names = Enum.map(:ets.match(__MODULE__, {{:_, :_, :_}, :"$1"}), fn [table] -> table.db_name end)

        Cloak.DataSource.all()
        |> Stream.filter(& &1.driver.supports_analyst_tables?)
        |> Stream.filter(&(&1.status == :online))
        |> Stream.flat_map(fn data_source ->
          data_source
          |> Cloak.DataSource.Connection.execute!(&data_source.driver.drop_unused_analyst_tables(&1, db_names))
          |> Enum.map(&{data_source, &1})
        end)
        |> Enum.each(fn {data_source, db_name} ->
          Logger.info("removed unused analyst table `#{db_name}` from `#{data_source.name}`")
        end)
      end,
      [node()],
      _retries = 5
    )
  end

  defp store_table_to_ets(table) do
    table_definition = table_definition(table)
    :ets.insert(__MODULE__, {{table.analyst, table.data_source.name, table.name}, table_definition})
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

  defp registration_info(table), do: Jason.encode!(update_in(table.data_source, & &1.name))

  defp register_table(registration_info) do
    with {:ok, table} <- table(registration_info),
         :ok <- store_table_in_database(table) do
      store_table_to_ets(table)
      {:ok, table}
    end
  end

  defp table(registration_info) do
    table =
      registration_info
      |> Jason.decode!()
      |> Map.take(~w(analyst name statement data_source db_name id_column store_info))
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
  def child_spec(_arg), do: Aircloak.ChildSpec.supervisor([gen_server(), periodic_cleaner()], strategy: :one_for_one)

  defp gen_server(), do: %{id: :gen_server, start: {__MODULE__, :start_link, []}}

  defp periodic_cleaner() do
    {Periodic,
     id: :periodic_cleaner,
     run: &drop_unused_analyst_tables/0,
     every: :timer.hours(1),
     timeout: :timer.minutes(1),
     overlap?: false}
  end

  @doc false
  def start_link(), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
