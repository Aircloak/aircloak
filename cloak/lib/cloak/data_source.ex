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
          name: "table name",
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

  # define returned data types and values
  @type t :: %{
    id: atom,
    driver: module,
    parameters: %{},
    tables: %{atom => table}
  }
  @type table :: %{
    name: String.t,
    user_id: String.t,
    ignore_unsupported_types: boolean,
    columns: [{column, data_type}]
  }
  @type num_rows :: non_neg_integer
  @type column :: String.t
  @type field :: String.t | integer | number | boolean | nil
  @type row :: [field]
  @type data_type :: :text | :integer | :real | :boolean | :timestamp | :time | :date | {:unsupported, String.t}
  @type query_result :: {num_rows, [column], [row]}


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
  # Supervisor callbacks
  #-----------------------------------------------------------------------------------------------------------

  use Supervisor

  @doc false
  def start_link() do
    Application.put_env(:cloak, :data_sources,
      Cloak.DeployConfig.fetch!("data_sources")
      |> atomize_keys()
      |> map_drivers()
    )

    case Supervisor.start_link(__MODULE__, :ok, name: __MODULE__) do
      {ok, pid} ->
        load_columns()
        {ok, pid}
      error ->
        error
    end
  end

  @doc false
  def init(:ok) do
    data_sources = Application.get_env(:cloak, :data_sources)
    # get the Supervisor spec for all defined data sources
    children = for {_, data_source} <- data_sources do
      driver = data_source[:driver]
      parameters = data_source[:parameters]
      driver.child_spec(data_source.id, Enum.to_list(parameters))
    end
    supervise(children, strategy: :one_for_one)
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

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

  @doc "Returns the list of columns for a specific table."
  @spec columns(t, atom) :: [{String.t, data_type}]
  def columns(data_source, table_id) do
    Map.fetch!(data_source.tables, table_id).columns
  end

  @doc """
  Execute a `select` query over the specified data source.
  Returns {RowCount, Columns, Rows}.
  """
  @spec select(t, Cloak.SqlQuery.t) :: {:ok, query_result} | {:error, any}
  def select(data_source, %{from: {:subquery, _}} = select_query) do
    driver = data_source.driver
    driver.select(data_source.id, select_query)
  end
  def select(data_source, %{columns: fields, from: table_identifier} = select_query) do
    driver = data_source.driver
    table_id = String.to_existing_atom(table_identifier)
    table = data_source[:tables][table_id]
    user_id = Map.fetch!(table, :user_id)
    table_name = Map.fetch!(table, :name)
    # insert the user_id column into the fields list, translate the table name and execute the `select` query
    driver.select(data_source.id, %{select_query | columns: [user_id | fields], from: table_name})
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

  defp map_drivers(data_sources) do
    Enum.map(data_sources, fn({data_source, params}) ->
      driver_module = case params[:driver] do
        "postgresql" -> Cloak.DataSource.PostgreSQL
        "dsproxy" -> Cloak.DataSource.DsProxy
        other -> raise("Unknown driver `#{other}` for data source `#{data_source}`")
      end

      {data_source, Map.merge(params, %{driver: driver_module, id: data_source})}
    end)
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
  defp load_columns() do
    data_sources = Application.get_env(:cloak, :data_sources)
    data_sources = for {id, data_source} <- data_sources, into: %{} do
      {id, load_data_source_columns(data_source)}
    end
    Application.put_env(:cloak, :data_sources, data_sources)
  end

  defp load_data_source_columns(data_source) do
    tables = for {table_id, table} <- data_source.tables, into: %{} do
      columns = load_table_columns(data_source, table)
      # verify the format of the columns list
      columns != [] or raise("Could not load columns for table '#{data_source.id}/#{table_id}'!")
      # extract user_id column and verify that it has the expected format
      user_id = table[:user_id]
      columns = case List.keytake(columns, user_id, 0) do
        {{^user_id, :integer}, data_columns} -> data_columns
        {{^user_id, :text}, data_columns} -> data_columns
        _ -> raise("Invalid user id column specified for table '#{data_source.id}/#{table_id}'!")
      end
      # check that we still have columns left in the list
      columns != [] or raise("No data columns found in table '#{data_source.id}/#{table_id}'!")
      # save the columns list into the table specification
      {table_id, Map.put(table, :columns, columns)}
    end
    # save the new table structure into the data source specification
    Map.put(data_source, :tables, tables)
  end

  defp load_table_columns(data_source, table) do
    columns = data_source.driver.get_columns(data_source.id, table.name)
    {supported, unsupported} = Enum.partition(columns, &supported?/1)

    validate_unsupported_columns(unsupported, data_source, table)
    supported
  end

  defp supported?({_name, {:unsupported, _db_type}}), do: false
  defp supported?({_name, _type}), do: true

  defp validate_unsupported_columns(unsupported, data_source, table) do
    cond do
      table[:ignore_unsupported_types] -> nil
      Enum.empty?(unsupported) -> nil
      true ->
        raise """
          The following columns in "#{data_source.id}/#{table[:name]}" have unsupported types:
          #{inspect(unsupported)}
          To ignore them set "ignore_unsupported_types: true" in your table settings
        """
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Test functions
  #-----------------------------------------------------------------------------------------------------------

  if Mix.env == :test do
    @doc false
    def register_test_table(table_id, table_name, user_id) do
      source = Application.get_env(:cloak, :data_sources)[:local]
      table = %{name: table_name, user_id: user_id}
      tables = Map.put(source[:tables], table_id, table)
      source = Map.put(source, :tables, tables)
      source = load_data_source_columns(source)
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
