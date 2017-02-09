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
  alias Cloak.DataSource.Parameters
  alias Cloak.Query.DataDecoder
  alias Cloak.Query.Runner.RuntimeError

  require Logger
  require Aircloak.DeployConfig

  # define returned data types and values
  @type t :: %{
    global_id: atom,
    driver: module,
    parameters: Cloak.DataSource.Driver.parameters,
    tables: %{atom => table}
  }
  @type table :: %{
    name: String.t, # table name as seen by the user
    db_name: String.t, # table name in the database
    user_id: String.t,
    ignore_unsupported_types: boolean,
    columns: [{column, data_type}],
    decoders: [DataDecoder.t],
    projection: %{table: String.t, primary_key: String.t, foreign_key: String.t} | nil
  }
  @type num_rows :: non_neg_integer
  @type column :: String.t
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
    |> Enum.reject(&(&1.driver == nil))
    |> Enum.map(&add_tables/1)
    |> store_to_cache()

  @doc "Returns the list of defined data sources."
  @spec all() :: [t]
  def all() do
    Application.get_env(:cloak, :data_sources)
  end

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
  def tables(data_source), do:
    Map.values(data_source.tables)

  @doc """
  Executes the specified 'select' query.

  Besides the query object, this methods also needs a result processing function
  for handling the stream of rows produced as a result of executing the query.

  The function returns the processed result. On error a `RuntimeError` is raised.
  """
  @spec select!(Query.t, result_processor) :: processed_result
  def select!(%{data_source: data_source} = select_query, result_processor) do
    driver = data_source.driver
    Logger.debug("Connecting to `#{data_source.global_id}` ...")
    connection = driver.connect!(data_source.parameters)
    try do
      Logger.debug("Selecting data ...")
      case driver.select(connection, select_query, result_processor) do
        {:ok, processed_result} -> processed_result
        {:error, reason} -> raise RuntimeError, message: reason
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
  def fetch(data_source_id) do
    Application.get_env(:cloak, :data_sources)
    |> Enum.find(&(&1.global_id === data_source_id))
    |> case do
      nil -> :error
      data_source -> {:ok, data_source}
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp to_data_source(data_source_configuration), do:
    data_source_configuration
    |> atomize_keys()
    |> generate_global_id()
    |> map_driver()

  defp map_driver(data_source) do
    driver_module = case data_source.driver do
      "postgresql" -> Cloak.DataSource.PostgreSQL
      "mysql" -> Cloak.DataSource.MySQL
      "odbc" -> Cloak.DataSource.ODBC
      "mongodb" -> Cloak.DataSource.MongoDB
      other ->
        Logger.error("Unknown driver `#{other}` for data source `#{data_source.global_id}`")
        nil
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

  defp generate_global_id(data) do
    # We want the global ID to take the form of:
    # <database-user>/<database-name>[-<aircloak data source marker>]@<database-host>[:<database-port>]
    # The data source marker is useful when we you want to force identical data sources to get
    # distinct global IDs. This can be used for exampel in staging and test environments.

    user = Parameters.get_one_of(data.parameters, ["uid", "user", "username"]) || "anon"
    database = Parameters.get_one_of(data.parameters, ["database"])
    host = Parameters.get_one_of(data.parameters, ["hostname", "server", "host"])

    if Enum.any?([database, host], &(is_nil(&1))) do
      raise "Misconfigured data source: database and hostname parameters are required."
    end

    marker = case Map.get(data, :marker) do
      nil -> ""
      marker -> "-#{marker}"
    end
    port = case Parameters.get_one_of(data.parameters, ["port"]) do
      nil -> ""
      port -> ":#{port}"
    end
    global_id = "#{user}/#{database}#{marker}@#{host}#{port}"
    Map.merge(data, %{global_id: global_id})
  end

  @doc false
  # load the columns list for all defined tables in all data sources
  def store_to_cache(data_sources) do
    Application.put_env(:cloak, :data_sources, data_sources)
  end

  @doc false
  def add_tables(data_source), do:
    Map.put(data_source, :tables, load_tables(data_source))

  defp load_tables(data_source) do
    driver = data_source.driver
    Logger.info("Loading tables from #{data_source.global_id} ...")
    connection = driver.connect!(data_source.parameters)
    try do
      data_source.tables
      |> Enum.map(fn ({table_id, table}) ->
        table
        |> Map.put(:columns, [])
        |> Map.put(:name, to_string(table_id))
        |> Map.put_new(:db_name, to_string(table_id))
        |> Map.put_new(:decoders, [])
      end)
      |> Enum.flat_map(&driver.load_tables(connection, &1))
      |> Enum.map(&normalize_table_map/1)
      |> Enum.map(&parse_columns(data_source, &1))
      |> Enum.filter(&(&1 != nil))
      |> Enum.map(&DataDecoder.init/1)
      |> Enum.map(&{String.to_atom(&1.name), &1})
      |> Enum.into(%{})
      |> resolve_projected_tables(data_source)
    after
      driver.disconnect(connection)
    end
  end

  defp normalize_table_map(table_map), do:
    Map.merge(%{user_id: nil, projection: nil}, table_map)

  defp parse_columns(data_source, table) do
    table.columns
    |> Enum.reject(&supported?/1)
    |> validate_unsupported_columns(data_source, table)
    case verify_columns(table) do
      {:ok, table} ->
        table
      {:error, reason} ->
        Logger.error("Error fetching columns for table #{data_source.global_id}/#{table.db_name}: #{reason}")
        nil
    end
  end

  defp verify_columns(table) do
    columns = for {name, _type} = column <- table.columns, do:
      if supported?(column), do: column, else: {name, :unknown}
    with :ok <- verify_user_id(table, columns) do
      case columns do
        [] -> {:error, "no data columns found in table"}
        [_|_] -> {:ok, %{table | columns: columns}}
      end
    end
  end

  defp verify_user_id(%{projection: projection}, _columns) when projection != nil, do: :ok
  defp verify_user_id(table, columns) do
    user_id = table.user_id
    case List.keyfind(columns, user_id, 0) do
      {^user_id, type} ->
        if type in [:integer, :text, :uuid, :real, :unknown] do
          :ok
        else
          {:error, "unsupported user id type: #{type}"}
        end
      _ ->
        columns_string =
          columns
          |> Enum.map(fn({column_name, _}) -> "`#{column_name}`" end)
          |> Enum.join(", ")
        {:error, "invalid user id column specified `#{user_id}`\n  columns: #{columns_string}"}
    end
  end

  defp supported?({_name, {:unsupported, _db_type}}), do: false
  defp supported?({_name, _type}), do: true

  defp validate_unsupported_columns([], _data_source, _table), do: nil
  defp validate_unsupported_columns(unsupported, data_source, table) do
    columns_string =
      unsupported
      |> Enum.map(fn({column_name, {:unsupported, type}}) -> "  #{column_name} :: #{inspect(type)}" end)
      |> Enum.join("\n")

    msg = "The following columns from table `#{table[:db_name]}` in data source `#{data_source.global_id}` " <>
      "have unsupported types:\n" <> columns_string

    if table[:ignore_unsupported_types] do
      Logger.warn(msg)
      nil
    else
      raise "#{msg}\nTo ignore these columns set `ignore_unsupported_types: true` in your table settings"
    end
  end

  defp resolve_projected_tables(tables_map, data_source), do:
    tables_map
    |> Map.keys()
    |> Enum.reduce(tables_map, &resolve_projected_table(&2, Map.fetch!(&2, &1), data_source))

  defp resolve_projected_table(tables_map, %{projection: nil}, _data_source), do:
    tables_map
  defp resolve_projected_table(tables_map, %{user_id: uid, columns: [{uid, _} | _]}, _data_source), do:
    # uid column is resolved
    tables_map
  defp resolve_projected_table(tables_map, table, data_source) do
    case validate_projection(tables_map, table) do
      {:ok, referenced_table} ->
        # recursively resolve dependent table (this allows the dependent table to be projected as well)
        tables_map = resolve_projected_table(tables_map, referenced_table, data_source)
        # refetch the table, since it's maybe updated
        referenced_table = Map.fetch!(tables_map, String.to_atom(referenced_table.name))

        uid_column_name = referenced_table.user_id
        uid_column = List.keyfind(referenced_table.columns, uid_column_name, 0)

        Map.put(tables_map, String.to_atom(table.name),
          table
          |> Map.put(:user_id, uid_column_name)
          |> update_in([:columns], &[uid_column | &1])
        )

      {:error, reason} ->
        Logger.error("Invalid projection in table `#{table.db_name}` in data source " <>
          "`#{data_source.global_id}`: #{reason}.")
        Map.delete(tables_map, String.to_atom(table.name))
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
    case List.keyfind(table.columns, table.projection.foreign_key, 0) do
      nil -> {:error, "foreign key column `#{table.projection.foreign_key}` doesn't exist"}
      _ -> :ok
    end
  end

  defp validate_primary_key(table, referenced_table) do
    {_, foreign_key_type} = List.keyfind(table.columns, table.projection.foreign_key, 0)
    case List.keyfind(referenced_table.columns, table.projection.primary_key, 0) do
      nil ->
        {
          :error,
          "primary key column `#{table.projection.primary_key}` not found in table " <>
            "`#{referenced_table.db_name}`"
        }
      {_, primary_key_type} when primary_key_type != foreign_key_type ->
        {
          :error,
          "foreign key type is `#{foreign_key_type}` while primary key type is `#{primary_key_type}`"
        }
      {_, ^foreign_key_type} -> :ok
    end
  end
end
