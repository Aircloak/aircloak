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
          ignore_unsupported_types: false
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

  The data source schema will also be sent to air, so it can be referenced by incoming tasks.
  """

  alias Cloak.Sql.Query
  alias Cloak.DataSource.Validations
  alias Cloak.DataSource.Parameters
  alias Cloak.Query.DataDecoder
  alias Cloak.Query.ExecutionError

  require Logger
  require Aircloak.DeployConfig

  # define returned data types and values
  @type t :: %{
    global_id: atom,
    name: String.t,
    driver: module,
    parameters: Cloak.DataSource.Driver.parameters,
    tables: %{atom => table},
    errors: [String.t]
  }
  @type table :: %{
    :name => String.t, # table name as seen by the user
    :db_name => String.t, # table name in the database
    :user_id => String.t,
    :ignore_unsupported_types => boolean,
    :columns => [column],
    :decoders => [DataDecoder.t],
    :projection => %{table: String.t, primary_key: String.t, foreign_key: String.t} | nil,
    optional(any) => any,
  }
  @type column :: %{name: String.t, type: data_type}
  @type num_rows :: non_neg_integer
  @type field :: String.t | number | boolean | nil
  @type row :: [field]
  @type data_type :: :text | :integer | :real | :boolean | :datetime | :time | :date | :uuid | :unknown
  @type query_result :: Enumerable.t
  @type processed_result :: any
  @type result_processor :: (query_result -> processed_result)


  #-----------------------------------------------------------------------------------------------------------
  # Driver behaviour
  #-----------------------------------------------------------------------------------------------------------

  defmodule Driver do
    @moduledoc "Specifies the interface for implementing the database specific data access operations."

    @type connection :: any
    @type parameters :: any

    @doc "Opens a new connection to the data store."
    @callback connect!(parameters) :: connection

    @doc "Closes the connection to the data store."
    @callback disconnect(connection) :: :ok

    @doc "Loads one or more table definitions from the data store."
    @callback load_tables(connection, Cloak.DataSource.table) :: [Cloak.DataSource.table]

    @doc "Driver specific implementation for the `DataSource.select` functionality."
    @callback select(connection, Query.t, Cloak.DataSource.result_processor)
      :: {:ok, Cloak.DataSource.processed_result} | {:error, any}

    @doc "Checks to see if the driver is able to handle all the SQL features used by the query."
    @callback supports_query?(Query.t) :: boolean
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Initializes the configured data sources.

  Starting is fault-tolerant: if some data sources can't be accessed, or if
  there are some errors in the configuration, the system will still start,
  using the valid datasources. Invalid data sources won't be accessible, but
  the system will log corresponding errors.
  """
  @spec start() :: :ok
  def start(), do:
    Aircloak.DeployConfig.fetch!("data_sources")
    |> Enum.map(&to_data_source/1)
    |> Enum.map(&add_tables/1)
    |> Validations.Name.check_for_duplicates()
    |> store_to_cache()

  @doc "Returns the list of defined data sources."
  @spec all() :: [t]
  def all(), do: Application.get_env(:cloak, :data_sources)

  @doc "Returns the table descriptor for the given table."
  @spec table(t, atom | String.t) :: table | nil
  def table(data_source, table_id) when is_atom(table_id), do: Map.fetch!(data_source.tables, table_id)
  def table(data_source, table_name) when is_binary(table_name) do
    case Enum.find(data_source.tables, fn({_id, table}) -> table.name == table_name end) do
      nil -> nil
      {_id, table} -> table
    end
  end

  @doc "Returns all table descriptors for the given data source."
  @spec tables(t) :: [table]
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

  @doc "Returns the datasource for the given id, raises if it's not found."
  @spec fetch!(String.t) :: t
  def fetch!(data_source_id) do
    {:ok, data_source} = fetch(data_source_id)
    data_source
  end

  @doc "Returns the datasource with the given id, or `:error` if it's not found."
  @spec fetch(String.t) :: {:ok, t} | :error
  def fetch(data_source_name) do
    Application.get_env(:cloak, :data_sources)
    |> Enum.find(&(&1.name === data_source_name))
    |> case do
      nil -> :error
      data_source -> {:ok, data_source}
    end
  end

  @doc "Raises an error when something goes wrong during data processing."
  @spec raise_error(String.t) :: no_return
  def raise_error(message), do: raise ExecutionError, message: message

  @doc "Creates the column entry in the table specification."
  @spec column(String.t, data_type) :: column
  def column(name, type), do:
    %{name: name, type: type}


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp to_data_source(data_source) do
    data_source
    |> atomize_keys()
    |> Map.put(:errors, [])
    |> Validations.Name.ensure_permitted()
    |> potentially_create_temp_name()
    |> generate_global_id()
    |> map_driver()
  end

  defp map_driver(data_source) do
    driver_module =
      case data_source.driver do
        "postgresql" -> Cloak.DataSource.PostgreSQL
        "mysql" -> Cloak.DataSource.MySQL
        "odbc" -> Cloak.DataSource.ODBC
        "mongodb" -> Cloak.DataSource.MongoDB
        other -> raise_error("Unknown driver `#{other}` for data source `#{data_source.name}`")
      end
    Map.put(data_source, :driver, driver_module)
  end

  defp atomize_keys(%{} = map) do
    for {key, value} <- map, into: %{}, do: {String.to_atom(key), atomize_keys(value)}
  end
  defp atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end
  defp atomize_keys(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> atomize_keys()
    |> List.to_tuple()
  end
  defp atomize_keys(other), do: other

  defp generate_global_id(data_source) do
    # We want the global ID to take the form of:
    # <database-user>/<database-name>[-<aircloak data source marker>]@<database-host>[:<database-port>]
    # The data source marker is useful when we you want to force identical data sources to get
    # distinct global IDs. This can be used for exampel in staging and test environments.

    user = Parameters.get_one_of(data_source.parameters, ["uid", "user", "username"]) || "anon"
    database = Parameters.get_one_of(data_source.parameters, ["database"])
    host = Parameters.get_one_of(data_source.parameters, ["hostname", "server", "host"])

    if Enum.any?([database, host], &(is_nil(&1))), do:
      raise_error("Invalid data source parameters: database and hostname are missing.")

    marker = case Map.get(data_source, :marker) do
      nil -> ""
      marker -> "-#{marker}"
    end
    port = case Parameters.get_one_of(data_source.parameters, ["port"]) do
      nil -> ""
      port -> ":#{port}"
    end
    global_id = "#{user}/#{database}#{marker}@#{host}#{port}"
    Map.merge(data_source, %{global_id: global_id})
  end

  @doc false
  # load the columns list for all defined tables in all data sources
  def store_to_cache(data_sources) do
    Application.put_env(:cloak, :data_sources, data_sources)
  end

  @doc false
  def add_tables(%{errors: existing_errors} = data_source) do
    Logger.info("Loading tables from #{data_source.name} ...")
    driver = data_source.driver
    try do
      connection = driver.connect!(data_source.parameters)
      try do
        data_source
        |> scan_tables(connection)
        |> resolve_projected_tables()
      after
        driver.disconnect(connection)
      end
    rescue
      error in ExecutionError ->
        message = "Connection error: #{Exception.message(error)}."
        Logger.error("Data source `#{data_source.name}`: #{message}")
        %{data_source | errors: existing_errors ++ [message], tables: %{}}
    end
  end

  defp scan_tables(%{errors: existing_errors} = data_source, connection) do
    {tables, errors} =
      Enum.reduce(data_source.tables, {[], []}, fn (table, {tables, errors}) ->
        try do
          {tables ++ load_tables(data_source, connection, table), errors}
        rescue
          error in ExecutionError ->
            {table_id, _} = table
            message = "Load error for table `#{table_id}`: #{Exception.message(error)}."
            Logger.error("Data source `#{data_source.name}`: #{message}")
            {tables, errors ++ [message]}
        end
      end)
    %{data_source | errors: existing_errors ++ errors, tables: Enum.into(tables, %{})}
  end

  defp load_tables(data_source, connection, {table_id, table}) do
    table_id = to_string(table_id)
    table =
      table
      |> Map.put(:columns, [])
      |> Map.put(:name, table_id)
      |> Map.put_new(:db_name, table_id)
      |> Map.put_new(:decoders, [])
      |> Map.put_new(:user_id, nil)
      |> Map.put_new(:projection, nil)

    data_source.driver.load_tables(connection, table)
    |> Enum.map(&parse_columns(data_source, &1))
    |> Enum.map(&DataDecoder.init/1)
    |> Enum.map(&{String.to_atom(&1.name), &1})
  end

  defp parse_columns(data_source, table) do
    table.columns
    |> Enum.reject(&supported?/1)
    |> validate_unsupported_columns(data_source, table)
    columns = for column <- table.columns, do:
      if supported?(column), do: column, else: %{column | type: :unknown}
    table = %{table | columns: columns}
    verify_columns(table)
    table
  end

  defp verify_columns(table) do
    verify_user_id(table)
    if table.columns == [], do: raise_error("no data columns found in table")
  end

  defp verify_user_id(%{projection: projection}) when projection != nil, do: :ok
  defp verify_user_id(table) do
    user_id = table.user_id
    case Enum.find(table.columns, &(&1.name == user_id)) do
      %{} = column ->
        unless column.type in [:integer, :text, :uuid, :real, :unknown], do:
          raise_error("unsupported user id type: #{column.type}")
      nil ->
        columns_string =
          table.columns
          |> Enum.map(&"`#{&1.name}`")
          |> Enum.join(", ")
        raise_error("invalid user id column specified: `#{user_id}` (columns: #{columns_string})")
    end
  end

  defp supported?(%{type: {:unsupported, _db_type}}), do: false
  defp supported?(_column), do: true

  defp validate_unsupported_columns([], _data_source, _table), do: nil
  defp validate_unsupported_columns(unsupported, data_source, table) do
    columns_string =
      unsupported
      |> Enum.map(fn({column_name, {:unsupported, type}}) -> "`#{column_name}`::#{inspect(type)}" end)
      |> Enum.join(", ")

    if table[:ignore_unsupported_types] do
      Logger.warn("The following columns from table `#{table[:db_name]}` in data source `#{data_source.name}` " <>
        "have unsupported types:\n" <> columns_string)
      nil
    else
      raise_error("unsupported types for columns: #{columns_string} "
        <> "(to ignore these columns set 'ignore_unsupported_types: true' in your table settings)")
    end
  end

  defp resolve_projected_tables(data_source), do:
    data_source.tables
    |> Map.keys()
    |> Enum.reduce(data_source, &resolve_projected_table(Map.fetch!(&2.tables, &1), &2))

  defp resolve_projected_table(%{projection: nil}, data_source), do: data_source
  defp resolve_projected_table(%{user_id: uid, columns: [{uid, _} | _]}, data_source), do:
    data_source # uid column is resolved
  defp resolve_projected_table(table, data_source) do
    case validate_projection(data_source.tables, table) do
      {:ok, referenced_table} ->
        # recursively resolve dependent table (this allows the dependent table to be projected as well)
        data_source = resolve_projected_table(referenced_table, data_source)
        # refetch the table, since it's maybe updated
        referenced_table = Map.fetch!(data_source.tables, String.to_atom(referenced_table.name))

        uid_column_name = referenced_table.user_id
        uid_column = Enum.find(referenced_table.columns, &(&1.name == uid_column_name))
        table =
          table
          |> Map.put(:user_id, uid_column_name)
          |> update_in([:columns], &[uid_column | &1])
        tables = Map.put(data_source.tables, String.to_atom(table.name), table)
        %{data_source | tables: tables}

      {:error, reason} ->
        message = "Projection error in table `#{table.name}`: #{reason}."
        Logger.error("Data source `#{data_source.name}`: #{message}")
        tables = Map.delete(data_source.tables, String.to_atom(table.name))
        %{data_source | tables: tables, errors: data_source.errors ++ [message]}
    end
  end

  defp validate_projection(tables_map, table) do
    with :ok <- validate_foreign_key(table),
         {:ok, referenced_table} <- validate_referenced_table(tables_map, table),
         :ok <- validate_primary_key(table, referenced_table),
    do: {:ok, referenced_table}
  end

  defp validate_referenced_table(tables_map, table) do
    case Map.fetch(tables_map, String.to_atom(table.projection.table)) do
      :error -> {:error, "referenced table `#{table.projection.table}` not found."}
      {:ok, referenced_table} -> {:ok, referenced_table}
    end
  end

  defp validate_foreign_key(table) do
    case Enum.find(table.columns, &(&1.name == table.projection.foreign_key)) do
      nil -> {:error, "foreign key column `#{table.projection.foreign_key}` doesn't exist"}
      _ -> :ok
    end
  end

  defp validate_primary_key(table, referenced_table) do
    foreign_key_type = Enum.find(table.columns, &(&1.name == table.projection.foreign_key)).type
    case Enum.find(referenced_table.columns, &(&1.name == table.projection.primary_key)) do
      nil ->
        {
          :error,
          "primary key column `#{table.projection.primary_key}` not found in table " <>
            "`#{referenced_table.db_name}`"
        }
      %{type: primary_key_type} when primary_key_type != foreign_key_type ->
        {
          :error,
          "foreign key type is `#{foreign_key_type}` while primary key type is `#{primary_key_type}`"
        }
      %{type: ^foreign_key_type} -> :ok
    end
  end

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
end
