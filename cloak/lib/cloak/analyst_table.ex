defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource
  alias Cloak.Sql.Query

  @max_concurrent_stores 5

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates or updates the analyst table in the database."
  @spec create_or_update(Query.analyst_id(), String.t(), String.t(), DataSource.t()) ::
          {:ok, registration_info :: String.t(), Query.described_columns()} | {:error, String.t()}
  def create_or_update(analyst, table_name, statement, data_source) do
    with {:ok, query} <- Cloak.AnalystTable.Compiler.compile(table_name, statement, data_source) do
      {db_name, store_info} = data_source.driver.prepare_analyst_table({analyst, table_name}, query)

      table = %{
        analyst: analyst,
        name: table_name,
        statement: statement,
        data_source_name: data_source.name,
        db_name: db_name,
        id_column: Cloak.Sql.Compiler.Helpers.id_column(query).name,
        store_info: store_info
      }

      start_table_store(table)
      {:ok, registration_info(table), Query.describe_selected(query)}
    end
  end

  @doc "Registers the analyst tables from the given registration infos, creating them in the database if needed."
  @spec register_tables([String.t()]) :: :ok
  def register_tables(registration_infos), do: GenServer.call(__MODULE__, {:register_tables, registration_infos})

  @doc "Returns the analyst table definition."
  @spec table_definition(Query.analyst_id(), String.t(), DataSource.t()) :: DataSource.Table.t() | nil
  def table_definition(analyst, table_name, data_source),
    do: Enum.find(analyst_tables(analyst, data_source), &(&1.name == table_name))

  @doc "Returns analyst tables for the given analyst in the given data source."
  @spec analyst_tables(Query.analyst_id(), DataSource.t()) :: [DataSource.Table.t()]
  def analyst_tables(analyst, data_source) do
    Enum.map(
      :ets.match(__MODULE__, {{analyst, data_source.name, :_}, :"$1"}),
      fn [table] -> table end
    )
  end

  @doc "Synchronously invoked the function as serialized, blocking all other store operations."
  @spec sync_serialized((() -> res), non_neg_integer | :infinity) :: res when res: var
  def sync_serialized(fun, timeout \\ :timer.seconds(5)),
    do: GenServer.call(__MODULE__, {:sync_serialized, fun}, timeout)

  # -------------------------------------------------------------------
  # GenServer and Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true, write_concurrency: true])
    {:ok, %{jobs: :queue.new()}}
  end

  @impl GenServer
  def handle_call({:sync_serialized, fun}, from, state),
    do: {:noreply, enqueue_job(state, :serialized, fn -> GenServer.reply(from, fun.()) end)}

  def handle_call({:store_table, table}, _from, state), do: {:reply, :ok, enqueue_store_table(state, table, true)}

  def handle_call({:register_tables, registration_infos}, _from, state) do
    :ets.delete_all_objects(__MODULE__)
    state = Enum.reduce(registration_infos, state, &register_table(&2, &1))
    {:reply, :ok, enqueue_job(state, :serialized, &drop_unused_analyst_tables/0)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(job_id, meta, _pid, reason, state) do
    if reason != :normal and match?({:store, _analyst, _table_name, _data_source_name}, job_id) do
      Logger.error("Error creating table: #{inspect(meta)}: #{inspect(reason)}")
      update_table_definition(meta, &%{&1 | status: :create_error})
    end

    {:noreply, maybe_start_job(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp stop_current_store(table),
    do: if(Parent.GenServer.child?(store_job_id(table)), do: Parent.GenServer.shutdown_child(store_job_id(table)))

  defp store_job_id(table), do: {:store, table.analyst, table.name, table.data_source_name}

  defp key(table), do: {table.analyst, table.data_source_name, table.name}

  defp store_table_definition(table, table_definition) do
    :ets.insert(__MODULE__, {key(table), table_definition})
    :ok
  end

  defp update_table_definition(table, updater_fun) do
    [{_key, table_definition}] = :ets.lookup(__MODULE__, key(table))
    store_table_definition(table, updater_fun.(table_definition))
    :ok
  end

  defp start_table_store(table), do: GenServer.call(__MODULE__, {:store_table, table})

  defp enqueue_store_table(state, table, recreate?) do
    stop_current_store(table)

    store_table_definition(
      table,
      DataSource.Table.new(table.name, table.id_column, %{db_name: table.db_name, status: :creating})
    )

    enqueue_job(state, store_job_id(table), table, fn -> store_table(table, recreate?) end)
  end

  defp store_table(table, recreate?) do
    with {:ok, data_source} <- fetch_data_source(table),
         :ok <- store_table_to_database(data_source, table, recreate?) do
      :ok
    else
      {:error, reason} ->
        Logger.error("Error creating table: #{inspect(table)}: #{reason}")
        update_table_definition(table, &%{&1 | status: :create_error})
    end
  end

  defp fetch_data_source(table) do
    with :error <- DataSource.fetch(table.data_source_name), do: {:error, "data source not found"}
  end

  defp store_table_to_database(data_source, table, recreate?) do
    DataSource.Connection.execute!(
      data_source,
      fn connection ->
        with :ok <- data_source.driver.store_analyst_table(connection, table.db_name, table.store_info, recreate?) do
          update_table_definition(
            table,
            fn table_definition ->
              [table_definition] = data_source.driver.load_tables(connection, %{table_definition | status: :created})
              table_definition
            end
          )
        end
      end
    )
  end

  defp drop_unused_analyst_tables() do
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
  end

  defp registration_info(table), do: Jason.encode!(table)

  defp register_table(state, registration_info) do
    table =
      registration_info
      |> Jason.decode!()
      |> Map.take(~w(analyst name statement data_source_name db_name id_column store_info))
      |> Aircloak.atomize_keys()

    case Cloak.DataSource.fetch(table.data_source_name) do
      {:ok, _} -> enqueue_store_table(state, table, false)
      :error -> state
    end
  end

  defp enqueue_job(state, id, meta \\ nil, fun),
    do: maybe_start_job(update_in(state.jobs, &:queue.in({id, meta, fun}, &1)))

  defp maybe_start_job(state) do
    with false <- serialized_job_running?(),
         true <- Parent.GenServer.num_children() < @max_concurrent_stores,
         {{:value, {job_id, job_meta, job_fun}}, queue} <- :queue.out(state.jobs),
         false <- job_id == :serialized and Parent.GenServer.num_children() > 0 do
      Parent.GenServer.start_child(%{
        id: job_id,
        meta: job_meta,
        start: {Task, :start_link, [job_fun]},
        shutdown: :brutal_kill
      })

      %{state | jobs: queue}
    else
      _ -> state
    end
  end

  defp serialized_job_running?(),
    do: Parent.GenServer.children() |> Stream.map(fn {_id, _pid, type} -> type end) |> Enum.any?(&(&1 == :serialized))

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Aircloak.ChildSpec.supervisor([gen_server(), periodic_cleaner()], strategy: :one_for_one)

  defp gen_server(), do: %{id: :gen_server, start: {__MODULE__, :start_link, []}}

  defp periodic_cleaner() do
    {Periodic,
     id: :periodic_cleaner,
     run: fn -> sync_serialized(&drop_unused_analyst_tables/0, :infinity) end,
     every: :timer.hours(1),
     timeout: :timer.minutes(59),
     overlap?: false}
  end

  @doc false
  def start_link(), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
