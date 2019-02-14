defmodule Cloak.DataSource do
  @moduledoc """
  This module handles access to user data in the cloak.
  The list of data sources available has to be specified in the configuration file.

  Example:
  config :cloak, data_sources: [
    data_source_id: [
      driver: DatabaseSpecificModule,
      parameters: ..., # database connection parameters
      tables: [
        table_id: [
          content_type: :private,
          db_name: "table name",
          keys: {
            "column_name" => key_type
          }
        ]
      ]
    ]

  A data source is accessed by id. It has to reference a module that implements the database specific access
  functionality and a parameters list, for connecting to the database server, that will be passed to that module.
  The database specific module needs to implement the `DataSource.Driver` behaviour.

  The data source must also specify the list of tables containing the data to be queried.
  A table is accessed by id. It must contain the name of the table in the database, the content protection type and
  the key columns identifying rows in other tables.

  During startup, the list of columns available in all defined tables is loaded and cached for later lookups.

  The keys field in each table can be used to list fields that refer to other tables. That way when a join
  condition of the form fk = pk will be added, no additional noise layers will be generated, resulting in less overall
  noise in those cases.

  The data source schema will also be sent to air, so it can be referenced by incoming tasks.
  """

  alias Cloak.DataSource.{Validations, Parameters, Driver, Table}
  alias Cloak.Query.ExecutionError

  require Logger
  require Aircloak.{DeployConfig, File}

  use GenServer

  # define returned data types and values
  @type t :: %{
          name: String.t(),
          driver: module,
          parameters: Driver.parameters(),
          driver_info: Driver.driver_info(),
          tables: %{atom => Table.t()},
          errors: [String.t()],
          status: :online | :offline,
          concurrency: non_neg_integer | nil,
          lcf_buckets_aggregation_limit: non_neg_integer | nil,
          statistics_anonymization: boolean | nil,
          # we need to store the initial tables and errors in case we need to re-scan the data source tables later
          initial_tables: %{atom => Table.t()},
          initial_errors: [String.t()],
          analyst_tables_enabled: boolean
        }

  @type num_rows :: non_neg_integer
  @type field :: String.t() | number | boolean | nil | Date.t() | Time.t() | NaiveDateTime.t()
  @type row :: [field]
  @type query_result :: Enumerable.t()
  @type processed_result :: any
  @type result_processor :: (query_result -> processed_result)

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Synchronously replaces the existing data source definitions with fresh ones loaded and initialized from the
  definitions configured on disk.

  If online, the Air will receive the updated data sources.
  """
  @spec reinitialize_all_data_sources() :: :ok
  def reinitialize_all_data_sources(), do: replace_all_data_source_configs(load_data_source_configs())

  @doc "Replaces the data source definitions maintained by the DataSource server"
  @spec replace_all_data_source_configs([t]) :: :ok
  def replace_all_data_source_configs(data_sources),
    do: GenServer.cast(__MODULE__, {:replace_data_sources, data_sources})

  @doc """
  Synchronously loads and initializes a data source given the path to the data source definition. If the data source
  doesn't yet exist it is added, and if it already exists it is replaced by the new one.

  If online, the Air will receive the updated data source.
  """
  @spec initialize_data_source_from_path(String.t()) :: :ok | :error
  def initialize_data_source_from_path(file_path) do
    file_path
    |> File.read!()
    |> Aircloak.Json.permissive_decode()
    |> case do
      {:ok, data_source_definition} ->
        [data_source] = initialize_data_source_configs([data_source_definition])
        replace_data_source_config(data_source)
        :ok

      {:error, reason} ->
        Logger.warn("Failed to initialize data source from path: #{file_path}: #{reason}")
        :error
    end
  end

  @doc """
  Validates that the Cloak can connect to the data source, and updates the online status of the
  data source. If the data source has been offline, it also has its table definitions refreshed.

  Changes to the data source or its online status will be propagated to the Air.
  """
  @spec perform_data_source_availability_checks() :: :ok
  def perform_data_source_availability_checks() do
    Cloak.DataSource.all()
    |> Task.async_stream(
      fn data_source ->
        updated_data_source = update_data_source_connectivity(data_source)

        if data_source.status != updated_data_source.status, do: replace_data_source_config(updated_data_source)
      end,
      timeout: :timer.minutes(30)
    )
    |> Stream.run()

    :ok
  end

  @doc "Returns the list of defined data sources."
  @spec all() :: [t]
  def all(), do: GenServer.call(__MODULE__, :all, :infinity)

  @doc "Returns the datasource with the given id, or `:error` if it's not found."
  @spec fetch(String.t()) :: {:ok, t} | :error
  def fetch(data_source_name) do
    GenServer.call(__MODULE__, {:get, data_source_name}, :infinity)
    |> case do
      nil -> :error
      data_source -> {:ok, data_source}
    end
  end

  @doc "Returns the table descriptor for the given table."
  @spec table(t, atom | String.t()) :: Table.t() | nil
  def table(data_source, table_id) when is_atom(table_id), do: Map.fetch!(data_source.tables, table_id)

  def table(data_source, table_name) when is_binary(table_name) do
    case Enum.find(data_source.tables, fn {_id, table} -> table.name == table_name end) do
      nil -> nil
      {_id, table} -> table
    end
  end

  @doc "Returns all table descriptors for the given data source."
  @spec tables(t) :: [Table.t()]
  def tables(data_source), do: Map.values(data_source.tables)

  @doc "Converts a data source config as found in a config into a data source"
  @spec config_to_datasources(Map.t()) :: [t]
  def config_to_datasources(config),
    do:
      config
      |> Enum.map(&to_data_source/1)
      |> Enum.reject(&disabled?/1)
      |> Validations.Name.check_for_duplicates()
      |> Enum.map(&save_init_fields/1)

  @doc "Expands a data source definition with tables derived from the database"
  @spec add_tables(t) :: t
  def add_tables(data_source) do
    Logger.info("Loading tables from #{data_source.name} ...")
    data_source = restore_init_fields(data_source)
    driver = data_source.driver

    try do
      Cloak.DataSource.Connection.execute!(
        data_source,
        fn connection ->
          data_source = Map.put(data_source, :driver_info, driver.driver_info(connection))

          with :ok <- check_analyst_tables_support(data_source),
               {:error, reason} <- data_source.driver.initialize_analyst_meta_table(connection),
               do: raise(ExecutionError, message: "Error initializing analyst meta table: #{reason}")

          data_source |> Table.load(connection) |> Map.put(:status, :online)
        end
      )
    rescue
      error in ExecutionError ->
        message = "Error loading data source: #{Exception.message(error)}."
        Logger.error("Data source `#{data_source.name}` is offline: #{Exception.message(error)}")
        add_error_message(%{data_source | tables: %{}, status: :offline}, message)
    end
  end

  @doc "Registers the calling process as a listener of data source changes."
  @spec subscribe_to_changes() :: :ok
  def subscribe_to_changes() do
    {:ok, _} = Registry.register(__MODULE__.ChangeListenersRegistry, :subscriber, nil)
    :ok
  end

  @doc "Returns true if analyst tables are supported and enabled on the given data source."
  @spec analyst_tables_supported?(t) :: boolean
  def analyst_tables_supported?(data_source) do
    check_analyst_tables_support(data_source) == :ok
  end

  @doc "Verifies if analyst tables are supported and enabled on the given data source."
  @spec check_analyst_tables_support(t) :: :ok | {:error, String.t()}
  def check_analyst_tables_support(data_source) do
    cond do
      not data_source.driver.supports_analyst_tables?() ->
        {:error, "analyst tables are not supported on this data source."}

      not data_source.analyst_tables_enabled ->
        {:error, "analyst tables are not enabled on this data source"}

      true ->
        :ok
    end
  end

  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, load_data_source_configs(), :hibernate}

  @impl GenServer
  def handle_call(:all, _from, data_sources) do
    {:reply, data_sources, data_sources}
  end

  def handle_call({:get, name}, _from, data_sources) do
    data_source = Enum.find(data_sources, &(&1.name === name))
    {:reply, data_source, data_sources}
  end

  @impl GenServer
  def handle_cast({:update_data_source, data_source}, old_data_sources) do
    updated_data_sources = replace_data_source(old_data_sources, data_source)
    publish_data_sources_change(updated_data_sources, old_data_sources)
    {:noreply, updated_data_sources, :hibernate}
  end

  def handle_cast({:replace_data_sources, new_data_sources}, old_data_sources) do
    publish_data_sources_change(new_data_sources, old_data_sources)
    {:noreply, new_data_sources, :hibernate}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp publish_data_sources_change(new_data_sources, old_data_sources) do
    if new_data_sources != old_data_sources do
      __MODULE__.ChangeListenersRegistry
      |> Registry.lookup(:subscriber)
      |> Enum.each(fn {subscriber_pid, _} -> send(subscriber_pid, {:data_sources_changed, new_data_sources}) end)
    end
  end

  defp replace_data_source(data_sources, data_source), do: Enum.uniq_by([data_source] ++ data_sources, & &1.name)

  defp load_data_source_configs() do
    Aircloak.DeployConfig.fetch!("data_sources")
    |> Cloak.DataSource.Utility.load_individual_data_source_configs()
    |> initialize_data_source_configs()
  end

  defp initialize_data_source_configs(data_source_configs) do
    data_sources = config_to_datasources(data_source_configs)

    data_sources
    |> Task.async_stream(&add_tables/1, timeout: :timer.minutes(30), ordered: true)
    |> Enum.zip(data_sources)
    |> Enum.map(&handle_add_tables_result/1)
    |> log_unclassified_columns()
  end

  defp handle_add_tables_result({{:ok, data_source}, _original_data_source}), do: data_source

  defp handle_add_tables_result({{:exit, _}, original_data_source}) do
    # If we came here, then the task running `add_tables` has crashed. We won't log the exit reason since it might
    # contain database password.
    Logger.error("Data source `#{original_data_source.name}` is offline")
    add_error_message(%{original_data_source | tables: %{}, status: :offline}, "connection error")
  end

  defp to_data_source(data_source) do
    data_source
    |> Aircloak.atomize_keys()
    |> standardize_key_lists()
    |> Map.put(:errors, [])
    |> Map.put(:status, nil)
    |> Map.put_new(:concurrency, nil)
    |> Map.put_new(:lcf_buckets_aggregation_limit, nil)
    |> Validations.Name.ensure_permitted()
    |> potentially_create_temp_name()
    |> Table.map_tables()
    |> map_driver()
  end

  defp map_driver(data_source) do
    case Cloak.DataSource.Utility.name_to_driver(data_source.driver) do
      {:ok, driver} ->
        Map.put(data_source, :driver, driver)

      {:error, :unknown} ->
        raise ExecutionError, message: "Unknown driver `#{data_source.driver}` for data source `#{data_source.name}`"
    end
  end

  defp save_init_fields(data_source),
    do:
      data_source
      |> Map.put(:initial_tables, data_source.tables)
      |> Map.put(:initial_errors, data_source.errors)

  defp restore_init_fields(data_source),
    do:
      data_source
      |> Map.put(:tables, data_source.initial_tables)
      |> Map.put(:errors, data_source.initial_errors)

  defp standardize_key_lists(data_source) do
    tables =
      for {name, table} <- data_source.tables, into: %{} do
        primary_keys =
          data_source.tables
          |> Map.values()
          |> Enum.filter(& &1[:projection])
          |> Enum.filter(&(&1.projection.table == to_string(name)))
          |> Enum.map(& &1.projection.primary_key)

        foreign_keys =
          if table[:projection],
            do: [table.projection.foreign_key],
            else: []

        keys = Enum.map(primary_keys ++ foreign_keys, &%{projection_key: &1}) |> Enum.uniq()
        {name, Map.put_new(table, :keys, keys)}
      end

    data_source = %{data_source | tables: tables}
    Map.put(data_source, :analyst_tables_enabled, Map.get(data_source, :analyst_tables_enabled, false))
  end

  @doc false
  def replace_data_source_config(data_source), do: GenServer.cast(__MODULE__, {:update_data_source, data_source})

  # We need a name for the data source in order for the Air to have something to attach
  # potential errors to. Therefore if none exists, we'll create a dummy name based on
  # the data source parameters.
  defp potentially_create_temp_name(data_source) do
    if Map.get(data_source, :name, "") === "" do
      user = Parameters.get_one_of(data_source.parameters, ["uid", "user", "username"]) || "anon"
      database = Parameters.get_one_of(data_source.parameters, ["database"])
      host = Parameters.get_one_of(data_source.parameters, ["hostname", "server", "host"])
      temp_name = "#{user}:#{database}@#{host}"
      Map.put(data_source, :name, temp_name)
    else
      data_source
    end
  end

  defp add_error_message(data_source, message), do: %{data_source | errors: [message | data_source.errors]}

  if Mix.env() == :prod do
    defp disabled?(data_source), do: explicitly_disabled?(data_source)
  else
    defp disabled?(data_source) do
      Enum.any?(
        [&explicitly_disabled?/1, &macos_disabled?/1, &default_schema_not_configured?/1],
        & &1.(data_source)
      )
    end

    defp macos_disabled?(data_source) do
      if :os.type() == {:unix, :darwin} and data_source.driver in [Cloak.DataSource.SAPHana] do
        ds_name = String.replace(to_string(data_source.driver), ~r/^Elixir\.Cloak\.DataSource\./, "")
        Logger.warn("Can't connect to #{ds_name} data source on macOS.")
        true
      else
        false
      end
    end

    defp default_schema_not_configured?(data_source) do
      if data_source.driver == Cloak.DataSource.SAPHana && is_nil(Cloak.DataSource.SAPHana.default_schema()) do
        Logger.warn("Default schema for SAP HANA not set. Skipping SAP HANA data source.")
        true
      else
        false
      end
    end
  end

  defp explicitly_disabled?(data_source) do
    env_data_sources = String.split(System.get_env("CLOAK_DATA_SOURCES") || "")
    not Enum.empty?(env_data_sources) and not Enum.member?(env_data_sources, data_source.name)
  end

  defp update_data_source_connectivity(%{status: :online} = data_source) do
    Cloak.DataSource.Connection.execute!(data_source, & &1, force_new_connection: true)
    data_source
  rescue
    error in ExecutionError ->
      message = "Connection error: #{Exception.message(error)}."
      Logger.error("Data source `#{data_source.name}` is offline: #{message}")
      add_error_message(%{data_source | tables: %{}, status: :offline}, message)
  end

  defp update_data_source_connectivity(%{status: :offline} = data_source), do: add_tables(data_source)

  defp log_unclassified_columns(data_sources) do
    Enum.each(data_sources, &log_unclassified_columns_for_data_source/1)
    data_sources
  end

  defp log_unclassified_columns_for_data_source(%{name: data_source_name} = data_source) do
    unclassified_column_tables = Cloak.DataSource.Isolators.unspecified_columns(data_source)

    unless unclassified_column_tables == %{} do
      unclassified_columns =
        unclassified_column_tables
        |> Enum.flat_map(&unclassified_table_columns_to_column_list/1)
        |> Enum.join("\n")

      Logger.info(
        "Automatic column classification has been turned off for one of more tables in data source " <>
          data_source_name <>
          ". The following unclassified columns will be treated as if they isolate users:\n" <> unclassified_columns
      )
    end
  end

  defp unclassified_table_columns_to_column_list({table_name, columns}), do: Enum.map(columns, &"- #{table_name}.#{&1}")

  # -------------------------------------------------------------------
  # Supervison tree callback
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    import Aircloak.ChildSpec

    supervisor(
      [
        supervisor(
          [
            registry(:duplicate, __MODULE__.ChangeListenersRegistry),
            Cloak.DataSource.Connection.Pool,
            gen_server(__MODULE__, nil, name: __MODULE__),
            Supervisor.child_spec(Cloak.DataSource.Isolators, id: :isolators),
            Supervisor.child_spec(Cloak.DataSource.Shadows, id: :shadows)
          ],
          strategy: :rest_for_one
        ),
        Cloak.DataSource.SerializingUpdater,
        Cloak.DataSource.PostgrexAutoRepair
      ],
      strategy: :one_for_one,
      name: Cloak.DataSource.Supervisor
    )
  end
end
