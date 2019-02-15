defmodule Cloak.AnalystTable do
  @moduledoc "Service for working with analyst tables"

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression}
  alias __MODULE__.{Compiler, Jobs}

  @type table :: %{
          analyst: Query.analyst_id(),
          name: String.t(),
          statement: String.t(),
          data_source_name: String.t(),
          db_name: String.t(),
          id_column: Expression.t(),
          store_info: String.t(),
          columns: [DataSource.Table.column()]
        }

  @type creation_state :: :succeeded | :failed

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates or updates the analyst table in the database."
  @spec create_or_update(
          String.t(),
          Query.analyst_id(),
          String.t(),
          String.t(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.view_map()
        ) :: {:ok, registration_info :: String.t(), Query.described_columns()} | {:error, String.t()}
  def create_or_update(air_name, analyst, table_name, statement, data_source, parameters \\ nil, views \\ %{}) do
    with {:ok, query} <-
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

      query_table =
        Enum.zip(query.column_titles, query.columns)
        |> Enum.map(fn {title, column} -> %Expression{column | alias: title} end)
        |> Cloak.Sql.Compiler.Helpers.create_table_from_columns(table_name)

      table = %{
        air_name: air_name,
        analyst: analyst,
        name: table_name,
        statement: statement,
        data_source_name: data_source.name,
        db_name: db_name,
        id_column: query_table.user_id,
        store_info: store_info,
        columns: query_table.columns
      }

      GenServer.call(__MODULE__, {:store_table, table})
      {:ok, registration_info(table), Query.describe_selected(query)}
    end
  end

  @doc "Drops the table from the database."
  @spec drop_table(String.t(), String.t()) :: :ok | {:error, String.t()}
  def drop_table(air_name, registration_info) do
    table = decode_registration_info(air_name, registration_info)

    with {:ok, data_source} <- Cloak.DataSource.fetch(table.data_source_name) do
      if data_source.status == :online do
        driver = data_source.driver

        DataSource.Connection.execute!(
          data_source,
          fn connection ->
            GenServer.call(__MODULE__, {:unregister_table, table})
            driver.drop_analyst_table(connection, table.db_name)
          end
        )
      else
        {:error, "data source is not connected"}
      end
    end
  end

  @doc "Registers the analyst tables from the given registration infos, creating them in the database if needed."
  @spec register_tables(String.t(), [String.t()]) :: :ok
  def register_tables(air_name, registration_infos),
    do: GenServer.call(__MODULE__, {:register_tables, air_name, registration_infos})

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
    {:ok, %{jobs: Jobs.new()}}
  end

  @impl GenServer
  def handle_call({:sync_serialized, fun}, from, state),
    do: {:noreply, enqueue_job(state, {:serialized, fn -> GenServer.reply(from, fun.()) end})}

  def handle_call({:store_table, table}, _from, state) do
    state = stop_job(state, create_table_job_id(table))
    store_table_definition(table, data_source_table(table, status: :creating))
    {:reply, :ok, enqueue_job(state, {:create_table, table})}
  end

  def handle_call({:register_tables, air_name, registration_infos}, _from, state) do
    tables = Enum.map(registration_infos, &decode_registration_info(air_name, &1))
    state = stop_obsolete_stores(state, tables)
    update_ets_table(tables)

    {:reply, :ok, state}
  end

  def handle_call({:unregister_table, table}, _from, state) do
    state = stop_job(state, create_table_job_id(table))
    :ets.delete(__MODULE__, key(table))
    {:reply, :ok, state}
  end

  @impl Parent.GenServer
  def handle_child_terminated(job_id, job, _pid, reason, state) do
    with true <- reason != :normal, {:create_table, table} <- job do
      Logger.error("Error creating table: #{inspect(table)}: #{inspect(reason)}")
      update_table_definition(table, &%{&1 | status: :create_error})
    end

    update_air(job_id, reason)

    {:noreply, state.jobs |> update_in(&Jobs.job_finished(&1, job)) |> start_next_jobs()}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_air(:serialized, _reason), do: :ok

  defp update_air({:create_table, job_meta}, reason) do
    status =
      case reason do
        :normal -> :succeeded
        _ -> :failed
      end

    Cloak.AirSocket.send_analyst_table_state_update(job_meta.analyst, job_meta.name, job_meta.data_source_name, status)
  end

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

  defp data_source_table(table, opts) do
    DataSource.Table.new(
      table.name,
      table.id_column,
      Map.merge(%{db_name: table.db_name, columns: table.columns}, Map.new(opts))
    )
  end

  defp store_table(table, store_fun) do
    with {:ok, data_source} <- fetch_data_source(table),
         :ok <- store_fun.(data_source, table) do
      Logger.info("Table `#{table.name}` created successfully")
      update_table_definition(table, &%{&1 | status: :created})
    else
      {:error, reason} ->
        Logger.error("Error creating table: #{inspect(table)}: #{reason}")
        update_table_definition(table, &%{&1 | status: :create_error})
    end
  end

  defp fetch_data_source(table) do
    with :error <- DataSource.fetch(table.data_source_name), do: {:error, "data source not found"}
  end

  defp store_table_to_database(data_source, table) do
    DataSource.Connection.execute!(
      data_source,
      &data_source.driver.create_or_update_analyst_table(
        &1,
        Jason.encode!(Map.take(table, [:air_name, :data_source_name, :analyst_id, :name])),
        table.db_name,
        table.store_info,
        table.statement
      )
    )
  end

  defp registration_info(table), do: Jason.encode!(table)

  defp decode_registration_info(air_name, registration_info) do
    registration_info
    |> Jason.decode!()
    |> Map.take(~w(analyst name statement data_source_name db_name id_column store_info columns))
    |> Aircloak.atomize_keys()
    |> update_in(
      [:columns],
      fn columns -> Enum.map(columns, &update_in(&1.type, fn type -> String.to_existing_atom(type) end)) end
    )
    |> Map.put(:air_name, air_name)
  end

  defp stop_obsolete_stores(state, new_tables) do
    storing_tables =
      Parent.GenServer.children()
      |> Enum.filter(&match?({_job_id, _pid, {:create_table, _table}}, &1))
      |> Enum.map(fn {_id, _pid, {:create_table, table}} -> table end)
      |> MapSet.new()

    obsolete_storing_tables = MapSet.difference(storing_tables, MapSet.new(new_tables))
    Enum.reduce(obsolete_storing_tables, state, &stop_job(&2, create_table_job_id(&1)))
  end

  defp update_ets_table(new_tables) do
    new_keys = new_tables |> Enum.map(&key/1) |> MapSet.new()
    known_keys = :ets.match(__MODULE__, {:"$1", :_}) |> List.flatten() |> MapSet.new()
    obsolete_keys = MapSet.difference(known_keys, new_keys)
    Enum.each(obsolete_keys, &:ets.delete(__MODULE__, &1))

    missing_tables =
      new_tables
      |> Enum.filter(&(not MapSet.member?(known_keys, key(&1))))
      |> Enum.filter(&match?({:ok, _}, Cloak.DataSource.fetch(&1.data_source_name)))

    Enum.each(
      missing_tables,
      fn table -> store_table_definition(table, data_source_table(table, status: :created)) end
    )
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
