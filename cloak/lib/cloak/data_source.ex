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
          db_name: "table name",
          user_id: "user id column name",
          ignore_unsupported_types: false,
          keys: ["another_table_id"]
        ]
      ]
    ]

  A data source is accessed by id. It has to reference a module that implements the database specific access
  functionality and a parameters list, for connecting to the database server, that will be passed to that module.
  The database specific module needs to implement the `DataSource.Driver` behaviour.

  The data source must also specify the list of tables containing the data to be queried.
  A table is accessed by id. It must contain the name of the table in the database and the column
  identifying the users (text or integer value).

  During startup, the list of columns available in all defined tables is loaded and cached for later lookups.
  If 'ignore_unsupported_types' is set to true then columns with types that aren't supported by the driver
  will be ignored at this point and unavailable for processing.

  The keys field in each table can be used to list fields that refer to other tables. That way when a join
  condition of the form fk = pk will be added, no additional noise layers will be generated, resulting in less overall
  noise in those cases. There is no need to add the projection (if any) to this list - it's included automatically.

  The data source schema will also be sent to air, so it can be referenced by incoming tasks.
  """

  alias Cloak.Sql.Query
  alias Cloak.DataSource.{Validations, Parameters, Driver, Table}
  alias Cloak.Query.ExecutionError

  require Logger
  require Aircloak.{DeployConfig, File}

  use GenServer, start: {__MODULE__, :start_link, []}

  # define returned data types and values
  @type t :: %{
    name: String.t,
    driver: module,
    driver_name: String.t,
    parameters: Driver.parameters,
    tables: %{atom => Table.t},
    errors: [String.t],
    status: :online | :offline,
    # we need to store the initial tables and errors in case we need to re-scan the data source tables later
    initial_tables: %{atom => Table.t},
    initial_errors: [String.t],
  }

  @type num_rows :: non_neg_integer
  @type field :: String.t | number | boolean | nil
  @type row :: [field]
  @type query_result :: Enumerable.t
  @type processed_result :: any
  @type result_processor :: (query_result -> processed_result)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Starts the handler process for data sources.

  Starting is fault-tolerant: if some data sources can't be accessed, or if
  there are some errors in the configuration, the system will still start,
  using the valid datasources. Invalid data sources won't be accessible, but
  the system will log corresponding errors.
  """
  def start_link() do
    initial_state = Aircloak.DeployConfig.fetch!("data_sources")
    |> load_individual_data_source_configs()
    |> config_to_datasources()
    |> Enum.map(&add_tables/1)
    GenServer.start_link(__MODULE__, initial_state, name: __MODULE__)
  end

  @doc "Returns the list of defined data sources."
  @spec all() :: [t]
  def all(), do: GenServer.call(__MODULE__, :all, :infinity)

  @doc "Returns the datasource with the given id, or `:error` if it's not found."
  @spec fetch(String.t) :: {:ok, t} | :error
  def fetch(data_source_name) do
    GenServer.call(__MODULE__, {:get, data_source_name}, :infinity)
    |> case do
      nil -> :error
      data_source -> {:ok, data_source}
    end
  end

  @doc "Returns the table descriptor for the given table."
  @spec table(t, atom | String.t) :: Table.t | nil
  def table(data_source, table_id) when is_atom(table_id), do: Map.fetch!(data_source.tables, table_id)
  def table(data_source, table_name) when is_binary(table_name) do
    case Enum.find(data_source.tables, fn({_id, table}) -> table.name == table_name end) do
      nil -> nil
      {_id, table} -> table
    end
  end

  @doc "Returns all table descriptors for the given data source."
  @spec tables(t) :: [Table.t]
  def tables(data_source), do: Map.values(data_source.tables)

  @doc """
  Executes the specified 'select' query.

  Besides the query object, this methods also needs a result processing function
  for handling the stream of rows produced as a result of executing the query.

  The function returns the processed result. On error a `ExecutionError` is raised.
  """
  @spec select!(Query.t, result_processor) :: processed_result
  def select!(%{data_source: data_source} = select_query, result_processor) do
    driver = data_source.driver
    Logger.debug("Connecting to `#{data_source.name}` ...")
    connection = driver.connect!(data_source.parameters)
    try do
      Logger.debug("Selecting data ...")
      case driver.select(connection, select_query, result_processor) do
        {:ok, processed_result} -> processed_result
        {:error, reason} -> raise_error(reason)
      end
    after
      Logger.debug("Disconnecting ...")
      driver.disconnect(connection)
    end
  end

  @doc "Raises an error when something goes wrong during data processing."
  @spec raise_error(String.t) :: no_return
  def raise_error(message), do: raise ExecutionError, message: message

  @doc "Returns the SQL dialect callback module."
  @spec sql_dialect_module(t) :: module | nil
  def sql_dialect_module(data_source), do:
    data_source.driver.sql_dialect_module(data_source[:parameters])

  @doc "Converts a data source config as found in a config into a data source"
  @spec config_to_datasources(Map.t) :: [t]
  def config_to_datasources(config), do:
    config
    |> Enum.map(&to_data_source/1)
    |> Enum.reject(&disabled_in_dev?/1)
    |> Validations.Name.check_for_duplicates()
    |> Enum.map(&save_init_fields/1)


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(data_sources) do
    activate_monitor_timer(self())
    {:ok, data_sources}
  end

  @impl GenServer
  def handle_call(:all, _from, data_sources) do
    {:reply, data_sources, data_sources}
  end
  def handle_call({:get, name}, _from, data_sources) do
    data_source = Enum.find(data_sources, & &1.name === name)
    {:reply, data_source, data_sources}
  end
  def handle_call({:update, data_sources}, _from, _old_data_sources) do
    {:reply, :ok, data_sources}
  end

  @impl GenServer
  def handle_info(:monitor, data_sources) do
    server_pid = self()
    Task.start_link(fn () ->
      old_status = Enum.map(data_sources, & &1.status)
      data_sources = Enum.map(data_sources, &check_data_source/1)
      new_status = Enum.map(data_sources, & &1.status)
      if new_status != old_status do
        update(data_sources)
        Logger.info("Data sources status changed, sending new configuration to air ...")
        Cloak.AirSocket.update_config(data_sources)
      end
      activate_monitor_timer(server_pid)
    end)
    {:noreply, data_sources}
  end


  # -------------------------------------------------------------------
  # Utility functions
  # -------------------------------------------------------------------

  # This is the legacy path where data sources where configured as a list of data source definitions inline
  @doc false
  def load_individual_data_source_configs(data_sources) when is_list(data_sources), do: data_sources
  def load_individual_data_source_configs(config_path) when is_binary(config_path) do
    case Aircloak.File.ls(config_path) do
      {:ok, data_source_config_files} ->
        data_source_config_files
        |> Enum.map(fn(file_name) ->
          path = Path.join(config_path, file_name)
          case Aircloak.File.read_config_file(path) do
            {:ok, data_source_definition} -> data_source_definition
            {:error, reason} ->
              Logger.error("Failed at reading datasource config from `#{path}`: #{reason}")
              nil
          end
        end)
        |> Enum.reject(& is_nil/1)
      {:error, reason} ->
        Logger.error("Failed at loading data sources configurations from `#{config_path}`. " <>
          "Reason: #{Aircloak.File.humanize_posix_error(reason)}.")
        []
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp activate_monitor_timer(pid) do
    interval = Application.get_env(:cloak, :data_source_monitor_interval)
    if interval != nil, do: Process.send_after(pid, :monitor, interval)
  end

  defp to_data_source(data_source) do
    data_source
    |> Aircloak.atomize_keys()
    |> standardize_key_lists()
    |> Map.put(:errors, [])
    |> Map.put(:status, nil)
    |> Validations.Name.ensure_permitted()
    |> potentially_create_temp_name()
    |> map_driver()
    |> validate_choice_of_encoding()
  end

  defp map_driver(data_source) do
    driver_name = data_source.driver
    driver_module = case driver_name do
      "mongodb" -> Cloak.DataSource.MongoDB
      "mysql" -> Cloak.DataSource.MySQL
      "odbc" -> Cloak.DataSource.ODBC
      "postgresql" -> Cloak.DataSource.PostgreSQL
      "sqlserver" -> Cloak.DataSource.SQLServer
      "sqlserver_tds" -> Cloak.DataSource.SQLServerTds
      "saphana" -> Cloak.DataSource.SAPHana
      other -> raise_error("Unknown driver `#{other}` for data source `#{data_source.name}`")
    end
    data_source
    |> Map.put(:driver, driver_module)
    |> Map.put(:driver_name, driver_name)
  end

  defp save_init_fields(data_source), do:
    data_source
    |> Map.put(:initial_tables, data_source.tables)
    |> Map.put(:initial_errors, data_source.errors)

  defp restore_init_fields(data_source), do:
    data_source
    |> Map.put(:tables, data_source.initial_tables)
    |> Map.put(:errors, data_source.initial_errors)

  defp standardize_key_lists(data_source) do
    tables = for {name, table} <- data_source.tables, into: %{} do
      keys = Map.get(table, :keys, [])

      primary_keys =
        data_source.tables
        |> Map.values()
        |> Enum.filter(& &1[:projection])
        |> Enum.filter(& &1.projection.table == to_string(name))
        |> Enum.map(& &1.projection.primary_key)

      foreign_keys = if table[:projection],
        do: [table.projection.foreign_key],
        else: []

      {name, Map.put(table, :keys, keys ++ primary_keys ++ foreign_keys)}
    end

    %{data_source | tables: tables}
  end

  @doc false
  def add_tables(data_source) do
    Logger.info("Loading tables from #{data_source.name} ...")
    data_source = restore_init_fields(data_source)
    driver = data_source.driver
    try do
      connection = driver.connect!(data_source.parameters)
      try do
        data_source
        |> Table.load(connection)
        |> Map.put(:status, :online)
      after
        driver.disconnect(connection)
      end
    rescue
      error in ExecutionError ->
        message = "Connection error: #{Exception.message(error)}."
        Logger.error("Data source `#{data_source.name}` is offline: #{message}")
        add_error_message(%{data_source | tables: %{}, status: :offline}, message)
    end
  end

  @doc false
  def update(data_sources), do:
    GenServer.call(__MODULE__, {:update, data_sources}, :infinity)

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

  defp check_data_source(%{status: :online} = data_source) do
    driver = data_source.driver
    try do
      data_source.parameters |> driver.connect!() |> driver.disconnect()
      data_source
    rescue
      error in ExecutionError ->
        message = "Connection error: #{Exception.message(error)}."
        Logger.error("Data source `#{data_source.name}` is offline: #{message}")
        add_error_message(%{data_source | tables: %{}, status: :offline}, message)
    end
  end
  defp check_data_source(%{status: :offline} = data_source) do
    add_tables(data_source)
  end

  defp validate_choice_of_encoding(%{parameters: %{encoding: encoding}} = data_source) do
    cond do
      encoding in ["latin1", "unicode", "utf8", "utf16", "utf32"] ->
        set_encoding(data_source, String.to_atom(encoding))
      encoding == "utf16-big" -> set_encoding(data_source, {:utf16, :big})
      encoding == "utf16-little" -> set_encoding(data_source, {:utf16, :little})
      encoding == "utf32-big" -> set_encoding(data_source, {:utf32, :big})
      encoding == "utf32-little" -> set_encoding(data_source, {:utf32, :little})
      true ->
        add_error_message(
          set_encoding(data_source, :latin1),
          "Unsupported encoding type: `#{encoding}`. Falling back to `latin1`"
        )
    end
  end
  defp validate_choice_of_encoding(data_source), do: data_source

  defp set_encoding(%{parameters: parameters} = data_source, encoding), do:
    %{data_source | parameters: Map.put(parameters, :encoding, encoding)}

  defp add_error_message(data_source, message), do:
    %{data_source | errors: [message | data_source.errors]}

  if Mix.env == :dev do
    defp disabled_in_dev?(%{driver: Cloak.DataSource.SAPHana}) do
      cond do
        is_nil(Cloak.DataSource.SAPHana.default_schema()) ->
          Logger.warn("Default schema for SAP HANA not set. Skipping SAP HANA data source.")
          true

        :os.type() == {:unix, :darwin} ->
          Logger.warn("Can't connect to SAP HANA data source on OS X.")
          true

        true -> false
      end
    end
    defp disabled_in_dev?(_data_source), do:
      false
  else
    defp disabled_in_dev?(_data_source), do:
      false
  end
end
