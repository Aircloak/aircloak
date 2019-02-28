defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource
  alias Cloak.Sql.Query
  alias __MODULE__.{Compiler, Jobs}

  @type t :: %{
          analyst: Query.analyst_id(),
          name: String.t(),
          statement: String.t(),
          air_name: String.t(),
          data_source_name: String.t(),
          db_name: String.t(),
          fingerprint: binary(),
          status: :creating | :created | :create_error
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates or updates the analyst table in the database."
  @spec create_or_update(
          Query.analyst_id(),
          String.t(),
          String.t(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.view_map()
        ) :: {:ok, Query.described_columns()} | {:error, String.t()}
  def create_or_update(analyst, table_name, statement, data_source, parameters \\ nil, views \\ %{}) do
    with {:ok, air_name} <- Cloak.Air.name(),
         {:ok, query} <-
           Compiler.compile(
             table_name,
             statement,
             analyst,
             data_source,
             parameters,
             views
           ) do
      with {:error, reason} <- DataSource.check_analyst_tables_support(data_source), do: raise(reason)

      hash = :crypto.hash(:sha256, :erlang.term_to_binary({air_name, data_source.name, analyst, table_name}))
      encoded_hash = Base.encode64(hash, padding: false)
      # make sure the name is not longer than 30 characters to avoid possible issues with some databases, such as Oracle
      db_name = String.slice("__ac_#{encoded_hash}", 0, 30)

      store_info = data_source.driver.prepare_analyst_table(db_name, query)

      table = %{
        air_name: air_name,
        analyst: analyst,
        name: table_name,
        statement: statement,
        data_source_name: data_source.name,
        db_name: db_name,
        store_info: store_info,
        fingerprint: :crypto.hash(:sha256, store_info),
        status: :creating
      }

      GenServer.call(__MODULE__, {:store_table, table})
      {:ok, Query.describe_selected(query)}
    end
  end

  @doc "Drops the table from the database."
  @spec drop_table(String.t(), String.t(), DataSource.t()) :: :ok | {:error, String.t()}
  def drop_table(analyst, table_name, data_source) do
    if data_source.status == :online do
      case find(analyst, table_name, data_source) do
        nil ->
          {:error, "table not found"}

        table ->
          db_execute!(
            data_source,
            fn %{connection: connection, driver: driver} ->
              Logger.info("Dropping analyst table #{log_table_info(table)}")
              GenServer.call(__MODULE__, {:unregister_table, table})
              driver.drop_analyst_table(connection, table.db_name)
            end
          )
      end
    else
      {:error, "data source is not connected"}
    end
  end

  @doc "Returns the analyst table definition."
  @spec find(Query.analyst_id(), String.t(), DataSource.t()) :: t | nil
  def find(analyst, table_name, data_source),
    do: Enum.find(analyst_tables(analyst, data_source), &(&1.name == table_name))

  @doc "Returns analyst tables for the given analyst in the given data source."
  @spec analyst_tables(Query.analyst_id(), DataSource.t()) :: [t]
  def analyst_tables(analyst, data_source) do
    case Cloak.Air.name() do
      {:error, _} ->
        []

      {:ok, air_name} ->
        Enum.map(
          :ets.match(__MODULE__, {{air_name, analyst, data_source.name, :_}, :"$1"}),
          fn [table] -> table end
        )
    end
  end

  @doc "Returns the cloak table structs which describe analyst tables for the given analyst in the given data source."
  @spec cloak_tables(Query.analyst_id(), DataSource.t(), Query.view_map()) :: [DataSource.Table.t()]
  def cloak_tables(analyst, data_source, views) do
    analyst
    |> analyst_tables(data_source)
    |> Stream.map(&to_cloak_table(&1, views))
    |> Stream.filter(&match?({:ok, _}, &1))
    |> Enum.map(fn {:ok, table} -> table end)
  end

  @doc "Returns the cloak table structure which describes the given table."
  @spec to_cloak_table(t, Query.view_map(), name: String.t()) :: {:ok, DataSource.Table.t()} | {:error, String.t()}
  def to_cloak_table(table, views, opts \\ []) do
    with {:ok, data_source} <- fetch_data_source(table),
         {:ok, query} <- Compiler.compile(table.name, table.statement, table.analyst, data_source, nil, views),
         do: {:ok, Query.to_table(query, Keyword.get(opts, :name, table.name), type: :analyst, db_name: table.db_name)}
  end

  @doc "Synchronously invoked the function as serialized, blocking all other store operations."
  @spec sync_serialized((() -> res), non_neg_integer | :infinity) :: res when res: var
  def sync_serialized(fun, timeout \\ :timer.seconds(5)),
    do: GenServer.call(__MODULE__, {:sync_serialized, fun}, timeout)

  @doc "Notifies the server process that data sources have been changed."
  @spec refresh() :: :ok
  def refresh(), do: GenServer.cast(__MODULE__, :refresh)

  # -------------------------------------------------------------------
  # GenServer and Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true, write_concurrency: true])
    {:ok, %{jobs: Jobs.new()}}
  end

  @impl GenServer
  def handle_call({:sync_serialized, fun}, from, state),
    do: {:noreply, enqueue_job(state, {:serialized, fn -> GenServer.reply(from, fun.()) end})}

  def handle_call({:store_table, table}, _from, state) do
    state = stop_job(state, create_table_job_id(table))
    store_table_definition(table)
    {:reply, :ok, enqueue_job(state, {:create_table, table})}
  end

  def handle_call({:unregister_table, table}, _from, state) do
    state = stop_job(state, create_table_job_id(table))
    :ets.delete(__MODULE__, key(table))
    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_cast(:refresh, state) do
    refresh_analyst_tables()
    {:noreply, state}
  end

  @impl Parent.GenServer
  def handle_child_terminated(_job_id, job, _pid, reason, state) do
    with {:create_table, table} <- job do
      table =
        if reason == :normal do
          Logger.info("Created analyst table #{log_table_info(table)}")
          %{table | status: :created}
        else
          Logger.error("Error creating analyst table: #{log_table_info(table)}: #{inspect(reason)}")
          %{table | status: :create_error}
        end

      store_table_definition(table)
      Cloak.AirSocket.send_analyst_table_state_update(table.analyst, table.name, table.data_source_name, table.status)
    end

    {:noreply, state.jobs |> update_in(&Jobs.job_finished(&1, job)) |> start_next_jobs()}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp stop_job(state, job_id) do
    case Parent.GenServer.child_meta(job_id) do
      {:ok, job} ->
        Parent.GenServer.shutdown_child(job_id)
        state.jobs |> update_in(&Jobs.job_finished(&1, job)) |> start_next_jobs()

      :error ->
        state
    end
  end

  defp create_table_job_id(table), do: {:create_table, Map.take(table, [:analyst, :name, :data_source_name])}

  defp key(table), do: {table.air_name, table.analyst, table.data_source_name, table.name}

  defp store_table_definition(table) do
    :ets.insert(__MODULE__, {key(table), table})
    :ok
  end

  defp store_table(table, store_fun) do
    with {:ok, data_source} <- fetch_data_source(table),
         :ok <- store_fun.(data_source, table) do
      :ok
    else
      {:error, reason} -> exit(reason)
    end
  end

  defp fetch_data_source(table) do
    with :error <- DataSource.fetch(table.data_source_name), do: {:error, "data source not found"}
  end

  defp store_table_to_database(data_source, table) do
    Logger.info("Creating analyst table #{log_table_info(table)}")
    db_execute!(data_source, & &1.driver.create_or_update_analyst_table(&1.connection, table, table.store_info))
  end

  defp enqueue_job(state, job),
    do: start_next_jobs(update_in(state.jobs, &Jobs.enqueue_job(&1, job)))

  defp start_next_jobs(state) do
    {next_jobs, jobs} = Jobs.next_jobs(state.jobs)
    Enum.each(next_jobs, &start_next_job(state, &1))
    %{state | jobs: jobs}
  end

  defp start_next_job(state, job) do
    job_fun = job_fun(job, state)

    {:ok, _} =
      Parent.GenServer.start_child(%{
        id: job_id(job),
        meta: job,
        start: {Task, :start_link, [job_fun]},
        shutdown: :brutal_kill
      })
  end

  defp job_id({:serialized, _fun}), do: :serialized
  defp job_id({:create_table, table}), do: create_table_job_id(table)

  defp job_fun({:serialized, fun}, _state), do: fun

  defp job_fun({:create_table, table}, state) do
    store_fun = Map.get(state, :store_fun, &store_table_to_database/2)
    fn -> store_table(table, store_fun) end
  end

  defp refresh_analyst_tables() do
    with {:ok, air_name} <- Cloak.Air.name() do
      Cloak.DataSource.all()
      |> Stream.filter(&(&1.status == :online && DataSource.analyst_tables_supported?(&1)))
      |> Task.async_stream(&refresh_data_source(air_name, &1),
        ordered: false,
        timeout: :timer.seconds(30),
        on_timeout: :kill_task
      )
      |> Stream.run()
    end
  end

  defp refresh_data_source(air_name, data_source) do
    tables = registered_analyst_tables(air_name, data_source)
    Enum.each(tables, &store_table_definition/1)

    # Now that we refreshed the tables from the database, we can remove ETS keys for the tables which are not registered
    # in the database. These keys will correspond to the tables which were deleted on another cloak.
    valid_keys = tables |> Stream.map(&key/1) |> MapSet.new()

    Enum.each(
      :ets.match(__MODULE__, {:"$1", :_}),
      fn [{ets_air_name, _analyst_id, data_source_name, _table_name} = key] ->
        if ets_air_name == air_name and data_source_name == data_source.name and not MapSet.member?(valid_keys, key),
          do: :ets.delete(__MODULE__, key)
      end
    )
  end

  @doc false
  def registered_analyst_tables(air_name, data_source),
    do: db_execute!(data_source, & &1.driver.registered_analyst_tables(&1.connection, air_name, &1.data_source_name))

  defp db_execute!(data_source, fun) do
    arg = %{data_source_name: data_source.name, driver: data_source.driver}
    DataSource.Connection.execute!(data_source, &fun.(Map.put(arg, :connection, &1)))
  end

  defp log_table_info(table), do: table |> Map.take(~w/air_name data_source_name analyst name db_name/a) |> inspect()

  # -------------------------------------------------------------------
  # Helpers for testing
  # -------------------------------------------------------------------

  @doc false
  def with_custom_store_fun(store_fun, fun) do
    full_store_fun = fn data_source, table ->
      store_fun.(fn -> store_table_to_database(data_source, table) end)
    end

    :sys.replace_state(__MODULE__, &Map.put(&1, :store_fun, full_store_fun))
    fun.()
  after
    :sys.replace_state(__MODULE__, &Map.delete(&1, :store_fun))
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
