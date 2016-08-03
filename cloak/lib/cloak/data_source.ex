defmodule Cloak.DataSource do
  @moduledoc """
  This module handles access to user data in the cloak.
  The list of data sources available has to be specified in the configuration file.

  Example:
  config :cloak, data_sources: [
    data_source_id: [
      driver: DatabaseSpecificModule,
      parameters: [
        ... # database connection parameters
      ],
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

  require Logger

  # define returned data types and values
  @type t :: %{
    id: atom,
    driver: module,
    parameters: %{},
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
  @type query_result :: [row]

  #-----------------------------------------------------------------------------------------------------------
  # Driver behaviour
  #-----------------------------------------------------------------------------------------------------------

  defmodule Driver do
    @moduledoc "Specifies the interface for implementing the database specific data access operations."

    @doc "Returns the Supervisor spec for monitoring worker processes for the given data source id and parameters."
    @callback child_spec(atom, Keyword.t) :: Supervisor.Spec.spec

    @doc "Retrieves the existing columns for the specified table name and data source id."
    @callback get_columns(Cloak.DataSource.t, String.t) :: [{String.t, DataSource.data_type}]

    @doc "Database specific implementation for the `DataSource.select` functionality."
    @callback select(Cloak.DataSource.t, Cloak.SqlQuery.t) ::
      {:ok, Cloak.DataSource.query_result} | {:error, any}
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Starts data source adapter supervision subtree.

  This function starts a datasource supervisor, and underneath it all data
  source adapters specified in the deployment configuration.

  Starting is fault-tolerant: if some data sources can't be accessed, or if
  there are some errors in the configuration, the system will still start,
  using the valid datasources. Invalid data sources won't be accessible, but
  the system will log corresponding errors.
  """
  @spec start_link() :: Supervisor.on_start
  def start_link() do
    data_sources =
      Cloak.DeployConfig.fetch!("data_sources")
      |> atomize_keys()
      |> Enum.map(&map_driver/1)

    child_specs =
      for {_, data_source} <- data_sources do
        data_source.driver.child_spec(data_source.id, Enum.to_list(data_source.parameters))
      end

    case Supervisor.start_link(child_specs, strategy: :one_for_one, name: __MODULE__) do
      {:ok, pid} ->
        cache_columns(data_sources)
        {:ok, pid}
      error ->
        error
    end
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
  @spec table(t, atom | String.t) :: table
  def table(data_source, table_id) when is_atom(table_id), do: Map.fetch!(data_source.tables, table_id)
  def table(data_source, table_name) when is_binary(table_name),
    do: table(data_source, String.to_existing_atom(table_name))

  @doc """
  Execute a `select` query over the specified data source.
  Returns {RowCount, Columns, Rows}.
  """
  @spec select(Cloak.SqlQuery.t) :: {:ok, query_result} | {:error, any}
  def select(%{data_source: data_source} = select_query), do:
    data_source.driver.select(data_source.id, select_query)

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

  defp map_driver({data_source, params}) do
    driver_module = case params[:driver] do
      "postgresql" -> Cloak.DataSource.PostgreSQL
      "dsproxy" -> Cloak.DataSource.DsProxy
      other -> raise("Unknown driver `#{other}` for data source `#{data_source}`")
    end

    {data_source, Map.merge(params, %{driver: driver_module, id: data_source})}
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

  # load the columns list for all defined tables in all data sources
  defp cache_columns(data_sources) do
    Application.put_env(:cloak, :data_sources,
      data_sources
      |> Stream.map(&data_source_with_columns/1)
      |> Enum.into(%{})
    )
  end

  defp data_source_with_columns({id, data_source}) do
    # A reasonably sized chunk to avoid opening too many simultaneous connections
    chunk_size = 50

    tables =
      data_source.tables
      |> Enum.chunk(chunk_size, chunk_size, [])
      |> Enum.map(&load_tables_columns(data_source, &1))
      |> Enum.reduce(%{}, &Map.merge/2)

    {id, Map.put(data_source, :tables, tables)}
  end

  defp load_tables_columns(data_source, tables) do
    Enum.map(tables, &Task.async(fn -> table_with_columns(data_source, &1) end))
    |> Enum.map(&Task.await/1)
    |> Stream.filter(&(&1 != nil))
    |> Enum.into(%{})
  end

  defp table_with_columns(data_source, {table_id, table}) do
    case load_table_columns(data_source, table) do
      {:ok, columns} ->
        {table_id, Map.merge(table, %{columns: columns, name: to_string(table_id)})}
      {:error, reason} ->
        Logger.error("Error fetching columns for table #{data_source.id}/#{table.db_name}: #{reason}")
        nil
    end
  end

  defp load_table_columns(data_source, table) do
    with {:ok, columns} <- do_load_columns(data_source, table) do
      {supported, unsupported} = Enum.partition(columns, &supported?/1)
      validate_unsupported_columns(unsupported, data_source, table)
      {:ok, supported}
    end
  end

  defp do_load_columns(data_source, table) do
    try do
      columns = data_source.driver.get_columns(data_source.id, table.db_name)
      with :ok <- verify_columns(table, columns), do: {:ok, columns}
    catch type, error ->
      {
        :error,
        Exception.format(type, error, :erlang.get_stacktrace())
      }
    end
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
        if type in [:integer, :text, :uuid] do
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
      source = Application.get_env(:cloak, :data_sources)[:local]
      table = %{db_name: table_name, user_id: user_id}
      tables = Map.put(source[:tables], table_id, table)
      source = Map.put(source, :tables, tables)
      {_id, source} = data_source_with_columns({:local, source})
      Application.put_env(:cloak, :data_sources, %{local: source})
    end

    @doc false
    def unregister_test_table(table_id) do
      source = Application.get_env(:cloak, :data_sources)[:local]
      tables = Map.delete(source[:tables], table_id)
      source = Map.put(source, :tables, tables)
      Application.put_env(:cloak, :data_sources, %{local: source})
    end

    @doc false
    def clear_test_tables() do
      source = Application.get_env(:cloak, :data_sources)[:local]
      source = Map.put(source, :tables, %{})
      Application.put_env(:cloak, :data_sources, %{local: source})
    end
  end
end
