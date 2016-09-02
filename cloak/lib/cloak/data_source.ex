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

  alias Cloak.Aql
  require Logger

  # define returned data types and values
  @type t :: %{
    id: atom,
    driver: module,
    parameters: Driver.parameters,
    tables: %{atom => table}
  }
  @type table :: %{
    name: String.t, # table name as seen by the user
    db_name: String.t, # table name in the database
    user_id: String.t,
    ignore_unsupported_types: boolean,
    columns: [{column, data_type}]
  }
  @type num_rows :: non_neg_integer
  @type column :: String.t
  @type field :: String.t | integer | number | boolean | nil
  @type row :: [field]
  @type data_type :: :text | :integer | :real | :boolean | :timestamp | :time | :date | {:unsupported, String.t}
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
    @callback connect(parameters) :: {:ok, connection} | {:error, any}

    @doc "Closes the connection to the data store."
    @callback disconnect(connection) :: :ok

    @doc "Retrieves the existing columns for the specified table name."
    @callback describe_table(connection, String.t) :: [{String.t, DataSource.data_type}]

    @doc "Driver specific implementation for the `DataSource.select` functionality."
    @callback select(connection, Aql.t, Cloak.DataSource.result_processor)
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
  def start() do
    data_sources =
      Cloak.DeployConfig.fetch!("data_sources")
      |> add_unique_id()
      |> atomize_keys()
      |> Enum.map(&map_driver/1)

    cache_columns(data_sources)
  end

  @doc "Returns the list of defined data sources."
  @spec all() :: [t]
  def all() do
    Map.values(Application.get_env(:cloak, :data_sources))
  end

  @doc "Returns the list of defined tables for a specific data source."
  @spec tables(t) :: [table]
  def tables(data_source) do
    Map.keys(data_source.tables)
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

  @doc """
  Executes the specified 'select' query.

  Besides the query object, this methods also needs a result processing function
  for handling the stream of rows produced as a result of executing the query.
  """
  @spec select(Aql.t, result_processor) :: {:ok, processed_result} | {:error, any}
  def select(%{data_source: data_source} = select_query, result_processor) do
    driver = data_source.driver
    with {:ok, connection} <- driver.connect(data_source.parameters) do
      try do
        driver.select(connection, select_query, result_processor)
      after
        driver.disconnect(connection)
      end
    end
  end

  @doc "Returns the datasource for the given id, raises if it's not found."
  @spec fetch!(atom) :: t
  def fetch!(data_source_id) do
    {:ok, data_source} = fetch(data_source_id)
    data_source
  end

  @doc "Returns the datasource with the given id, or `:error` if it's not found."
  @spec fetch(atom) :: {:ok, t} | :error
  def fetch(data_source_id) do
    Application.get_env(:cloak, :data_sources)
    |> Map.fetch(data_source_id)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp map_driver({id, data_source}) do
    driver_module = case data_source.driver do
      "postgresql" -> Cloak.DataSource.PostgreSQL
      "mysql" -> Cloak.DataSource.MySQL
      "dsproxy" -> Cloak.DataSource.DsProxy
      "odbc" -> Cloak.DataSource.ODBC
      other -> raise("Unknown driver `#{other}` for data source `#{id}`")
    end
    {id, Map.merge(data_source, %{driver: driver_module, id: id})}
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

  defp add_unique_id(data_sources) do
    for data_source <- data_sources, into: %{}, do: add_unique_id_to_data_source(data_source)
  end

  defp add_unique_id_to_data_source({data_source_name, data}) do
    # Useful when we want to make the same data source appear multiple times
    # as if it was distinct data sources. Used in staging and testing environments.
    aircloak_data_source_marker = data["data_source_marker"] || ""
    unique_id_data = {aircloak_data_source_marker, data["parameters"]} |> :erlang.term_to_binary()
    # MD5 is perfectly fine here, as the hash doesn't serve any other purpose than generating
    # a single ID based on the data. Of course collisions can be constructed, but doing so is
    # not in anyone's interest, and furthermore would not compromise any user data.
    unique_id = :crypto.hash(:md5, unique_id_data) |> Base.encode64() |> String.to_atom()
    {data_source_name, Map.merge(data, %{"unique_id" => unique_id})}
  end

  # load the columns list for all defined tables in all data sources
  defp cache_columns(data_sources) do
    data_sources = for data_source <- data_sources, into: %{}, do: data_source_with_columns(data_source)
    Application.put_env(:cloak, :data_sources, data_sources)
  end

  defp data_source_with_columns({id, data_source}) do
    tables = load_tables_columns(data_source) |> Enum.into(%{})
    {id, Map.put(data_source, :tables, tables)}
  end

  defp load_tables_columns(data_source) do
    driver = data_source.driver
    with {:ok, connection} <- driver.connect(data_source.parameters) do
      try do
        for table <- data_source.tables, into: %{}, do: table_with_columns(data_source, connection, table)
      after
        driver.disconnect(connection)
      end
    else
      {:error, reason} ->
        Logger.error("Error connecting to #{data_source.id}: #{reason}")
        nil
    end
  end

  defp table_with_columns(data_source, connection, {table_id, table}) do
    case load_table_columns(data_source, connection, table) do
      {:ok, columns} ->
        {table_id, Map.merge(table, %{columns: columns, name: to_string(table_id)})}
      {:error, reason} ->
        Logger.error("Error fetching columns for table #{data_source.id}/#{table.db_name}: #{reason}")
        nil
    end
  end

  defp load_table_columns(data_source, connection, table) do
    with {:ok, columns} <- do_load_columns(data_source, connection, table) do
      {supported, unsupported} = Enum.partition(columns, &supported?/1)
      validate_unsupported_columns(unsupported, data_source, table)
      {:ok, supported}
    end
  end

  defp do_load_columns(data_source, connection, table) do
    columns = data_source.driver.describe_table(connection, table.db_name)
    with :ok <- verify_columns(table, columns), do: {:ok, columns}
  end

  defp verify_columns(table, columns) do
    with :ok <- verify_user_id(table, columns) do
      case columns do
        [] -> {:error, "no data columns found in table"}
        [_|_] -> :ok
      end
    end
  end

  defp verify_user_id(table, columns) do
    user_id = table.user_id
    case List.keyfind(columns, user_id, 0) do
      {^user_id, type} ->
        if type in [:integer, :text, :uuid, :real] do
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
    table_string = "#{data_source.id}/#{table[:name]}"

    columns_string =
      unsupported
      |> Enum.map(fn({column_name, {:unsupported, type}}) -> "  #{column_name} :: #{type}" end)
      |> Enum.join("\n")

    msg =
      "The following columns in `#{table_string}` have unsupported types and will be ignored:\n" <>
      columns_string

    if table[:ignore_unsupported_types] do
      Logger.warn(msg)
      nil
    else
      raise "#{msg}\nTo ignore these columns set `ignore_unsupported_types: true` in your table settings"
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Test functions
  #-----------------------------------------------------------------------------------------------------------

  if Mix.env == :test do
    @doc false
    def register_test_table(table_id, table_name, user_id) do
      sources = for {id, source} <- Application.get_env(:cloak, :data_sources), into: %{} do
        table = %{db_name: table_name, user_id: user_id}
        tables = Map.put(source[:tables], table_id, table)
        {id, Map.put(source, :tables, tables)} |> data_source_with_columns()
      end
      Application.put_env(:cloak, :data_sources, sources)
    end

    @doc false
    def unregister_test_table(table_id) do
      sources = for {id, source} <- Application.get_env(:cloak, :data_sources), into: %{} do
        tables = Map.delete(source[:tables], table_id)
        {id, Map.put(source, :tables, tables)}
      end
      Application.put_env(:cloak, :data_sources, sources)
    end

    @doc false
    def clear_test_tables() do
      sources = for {id, source} <- Application.get_env(:cloak, :data_sources), into: %{} do
        {id, Map.put(source, :tables, %{})}
      end
      Application.put_env(:cloak, :data_sources, sources)
    end
  end
end
