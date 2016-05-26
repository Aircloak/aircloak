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
  @type num_rows :: non_neg_integer
  @type column :: String.t
  @type row :: [String.t]
  @type data_type :: :text | :integer | :real | :boolean | :timestamp |
    :time | :date | :interval | {:unsupported, String.t}
  @type query_result :: {num_rows, [column], [row]}


  #-----------------------------------------------------------------------------------------------------------
  # Driver behaviour
  #-----------------------------------------------------------------------------------------------------------

  defmodule Driver do
    @moduledoc "Specifies the interface for implementing the database specific data access operations."

    @doc "Returns the Supervisor spec for monitoring worker processes for the given data source id and parameters."
    @callback child_spec(atom, Keyword.t) :: Supervisor.Spec.spec

    @doc "Retrieves the existing columns for the specified table name and data source id."
    @callback get_columns(atom, String.t) :: [[{String.t, DataSource.data_type}]]

    @doc "Database specific implementation for the `DataSource.select` functionality."
    @callback select(atom, Cloak.SqlQuery.t) :: {:ok, Cloak.DataSource.query_result} | {:error, any}
  end


  #-----------------------------------------------------------------------------------------------------------
  # Supervisor callbacks
  #-----------------------------------------------------------------------------------------------------------

  use Supervisor

  @doc false
  def start_link() do
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
    children = for {source_id, data_source} <- data_sources do
      driver = data_source[:driver]
      parameters = data_source[:parameters]
      driver.child_spec(source_id, parameters)
    end
    supervise(children, strategy: :one_for_one)
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the list of defined data sources."
  @spec all() :: [atom]
  def all() do
    data_sources = Application.get_env(:cloak, :data_sources)
    Keyword.keys(data_sources)
  end

  @doc "Returns the list of defined tables for a specific data source."
  @spec tables(atom) :: [atom]
  def tables(source_id) do
    data_sources = Application.get_env(:cloak, :data_sources)
    Keyword.keys(data_sources[source_id][:tables])
  end

  @doc "Returns the list of columns for a specific table."
  @spec columns(atom, atom) :: [{String.t, data_type}]
  def columns(source_id, table_id) do
    data_sources = Application.get_env(:cloak, :data_sources)
    data_sources[source_id][:tables][table_id][:columns]
  end

  @doc """
  Execute a `select` query over the specified data source.
  Returns {RowCount, Columns, Rows}.
  """
  @spec select(atom, Cloak.SqlQuery.t) :: {:ok, query_result} | {:error, any}
  def select(source_id, %{select: fields, from: table_identifier} = select_query) do
    data_sources = Application.get_env(:cloak, :data_sources)
    data_source = data_sources[source_id]
    driver = data_source[:driver]
    table_id = String.to_existing_atom(table_identifier)
    table = data_source[:tables][table_id]
    user_id = Keyword.fetch!(table, :user_id)
    table_name = Keyword.fetch!(table, :name)
    # insert the user_id column into the fields list, translate the table name and execute the `select` query
    driver.select(source_id, %{select_query | select: [user_id | fields], from: table_name})
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  # load the columns list for all defined tables in all data sources
  defp load_columns() do
    data_sources = Application.get_env(:cloak, :data_sources)
    data_sources = for {id, data_source} <- data_sources, do: {id, load_columns(id, data_source)}
    Application.put_env(:cloak, :data_sources, data_sources)
  end

  defp load_columns(source_id, data_source) do
    tables = for {table_id, table} <- data_source[:tables] do
      columns = load_columns(source_id, data_source, table)
      # verify the format of the columns list
      columns != [] or raise("Could not load columns for table '#{source_id}/#{table_id}'!")
      # extract user_id column and verify that it has the expected format
      user_id = table[:user_id]
      columns = case List.keytake(columns, user_id, 0) do
        {{^user_id, :integer}, data_columns} -> data_columns
        {{^user_id, :text}, data_columns} -> data_columns
        _ -> raise("Invalid user id column specified for table '#{source_id}/#{table_id}'!")
      end
      # check that we still have columns left in the list
      columns != [] or raise("No data columns found in table '#{source_id}/#{table_id}'!")
      # save the columns list into the table specification
      {table_id, Keyword.put(table, :columns, columns)}
    end
    # save the new table structure into the data source specification
    Keyword.put(data_source, :tables, tables)
  end

  defp load_columns(source_id, data_source, table) do
    columns = data_source[:driver].get_columns(source_id, table[:name])
    {supported, unsupported} = Enum.partition(columns, &supported?/1)

    validate_unsupported_columns(unsupported, source_id, table)
    supported
  end

  defp supported?({_name, {:unsupported, _db_type}}), do: false
  defp supported?({_name, _type}), do: true

  defp validate_unsupported_columns(unsupported, source_id, table) do
    cond do
      table[:ignore_unsupported_types] -> nil
      Enum.empty?(unsupported) -> nil
      true ->
        raise """
          The following columns in "#{source_id}/#{table[:name]}" have unsupported types:
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
    def register_test_table(table_name, user_id) do
      source = Application.get_env(:cloak, :data_sources)[:local]
      table_id = String.to_atom(table_name)
      table = [name: table_name, user_id: user_id]
      tables = Keyword.put(source[:tables], table_id, table)
      source = Keyword.put(source, :tables, tables)
      source = load_columns(:local, source)
      Application.put_env(:cloak, :data_sources, [local: source])
    end

    @doc false
    def unregister_test_table(table_name) do
      source = Application.get_env(:cloak, :data_sources)[:local]
      table_id = String.to_existing_atom(table_name)
      tables = Keyword.delete(source[:tables], table_id)
      source = Keyword.put(source, :tables, tables)
      Application.put_env(:cloak, :data_sources, [local: source])
    end

    @doc false
    def clear_test_tables() do
      source = Application.get_env(:cloak, :data_sources)[:local]
      source = Keyword.put(source, :tables, [])
      Application.put_env(:cloak, :data_sources, [local: source])
    end
  end
end
