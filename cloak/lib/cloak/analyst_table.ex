defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource

  @max_concurrent_stores 5

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores the analyst table to the database."
  @spec store(Cloak.Sql.Query.analyst_id(), String.t(), String.t(), DataSource.t()) ::
          {:ok, registration_info :: String.t()} | {:error, String.t()}
  def store(analyst, table_name, statement, data_source) do
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
      {:ok, registration_info(table)}
    end
  end

  @doc "Registers the analyst tables from the given registration infos, creating them in the database if needed."
  @spec register_tables([String.t()]) :: :ok
  def register_tables(registration_infos) do
    Enum.each(registration_infos, &register_table/1)
    GenServer.cast(__MODULE__, {:async_serialized, &drop_unused_analyst_tables/0})
  end

  @doc "Returns the analyst table definition."
  @spec table_definition(Cloak.Sql.Query.analyst_id(), String.t(), DataSource.t()) :: DataSource.Table.t() | nil
  def table_definition(analyst, table_name, data_source),
    do: Enum.find(analyst_tables(analyst, data_source), &(&1.name == table_name))

  @doc "Returns analyst tables for the given analyst in the given data source."
  @spec analyst_tables(Cloak.Sql.Query.analyst_id(), DataSource.t()) :: [DataSource.Table.t()]
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
  def handle_cast({:async_serialized, fun}, state),
    do: {:noreply, enqueue_job(state, :serialized, fun)}

  @impl GenServer
  def handle_call({:sync_serialized, fun}, from, state),
    do: {:noreply, enqueue_job(state, :serialized, fn -> GenServer.reply(from, fun.()) end)}

  def handle_call({:store_table, table}, _from, state) do
    child_id = {:store, table.name}
    if Parent.GenServer.child?(child_id), do: Parent.GenServer.shutdown_child(child_id)

    table_definition = DataSource.Table.new(table.name, table.id_column, %{db_name: table.db_name, status: :creating})
    :ets.insert(__MODULE__, {{table.analyst, table.data_source_name, table.name}, table_definition})

    {:reply, :ok, enqueue_job(state, child_id, fn -> store_table(table) end)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_job(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_table_store(table), do: GenServer.call(__MODULE__, {:store_table, table})

  defp store_table(table) do
    with {:ok, data_source} <- DataSource.fetch(table.data_source_name) do
      DataSource.Connection.execute!(
        data_source,
        fn connection ->
          [{_key, table_definition}] = :ets.lookup(__MODULE__, {table.analyst, table.data_source_name, table.name})

          table_definition =
            case data_source.driver.store_analyst_table(connection, table.db_name, table.store_info) do
              :ok ->
                [table_definition] = data_source.driver.load_tables(connection, table_definition)
                %{table_definition | status: :created}

              {:error, reason} ->
                Logger.error("Error creating table: #{inspect(table)}: #{reason}")
                %{table_definition | status: :create_error}
            end

          :ets.insert(__MODULE__, {{table.analyst, table.data_source_name, table.name}, table_definition})
        end
      )
    end
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

  defp register_table(registration_info) do
    registration_info
    |> Jason.decode!()
    |> Map.take(~w(analyst name statement data_source_name db_name id_column store_info))
    |> Aircloak.atomize_keys()
    |> start_table_store()
  end

  defp enqueue_job(state, id, fun), do: maybe_start_job(update_in(state.jobs, &:queue.in({id, fun}, &1)))

  defp maybe_start_job(state) do
    with false <- serialized_job_running?(),
         true <- Parent.GenServer.num_children() < @max_concurrent_stores,
         {{:value, {job_id, job_fun}}, queue} <- :queue.out(state.jobs),
         false <- job_id == :serialized and Parent.GenServer.num_children() > 0 do
      Parent.GenServer.start_child(%{id: job_id, start: {Task, :start_link, [job_fun]}, shutdown: :brutal_kill})
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
