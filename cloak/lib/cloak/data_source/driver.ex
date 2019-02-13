defmodule Cloak.DataSource.Driver do
  @moduledoc "Specifies the interface for implementing the database specific data access operations."

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.Table

  @doc "Returns the configured database connection timeout."
  @spec connect_timeout() :: pos_integer
  def connect_timeout(), do: Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:connect_timeout)

  @doc "Returns the configured time for keeping the connection alive for potential reuse."
  @spec connection_keep_time() :: pos_integer
  def connection_keep_time(), do: Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:connection_keep_time)

  @doc "Returns the configured maximum timeout for a database operation."
  @spec timeout() :: pos_integer
  def timeout(), do: Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:timeout)

  @doc "Returns the configured batch size for a select operation."
  @spec batch_size() :: pos_integer
  def batch_size(), do: Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:batch_size)

  @type connection :: pid()
  @type parameters :: any
  @type driver_info :: any

  @doc "Returns an atom describing the query dialect of this specific driver implementation."
  @callback sql_dialect_module() :: module | nil

  @doc """
  Opens a new connection to the data store.

  The returned connection has to be a child process of the current process.
  """
  @callback connect(parameters) :: {:ok, connection} | {:error, String.t()}

  @doc "Closes the connection to the data store."
  @callback disconnect(connection) :: :ok

  @doc "Loads one or more table definitions from the data store."
  @callback load_tables(connection, Table.t()) :: [Table.t()]

  @doc "Driver specific implementation for the retrieving chunks of rows."
  @callback select(connection, Query.t(), Cloak.DataSource.result_processor()) ::
              {:ok, Cloak.DataSource.processed_result()} | {:error, any}

  @doc "Checks to see if the driver is able to handle all the SQL features used by the query."
  @callback supports_query?(Query.t()) :: boolean

  @doc "Checks to see if the driver is able to handle specified function natively."
  @callback supports_function?(Expression.t(), Cloak.DataSource.t()) :: boolean

  @doc "Returns the driver specific information to be stored inside the data source structure."
  @callback driver_info(connection) :: driver_info

  @doc "Returns true if the driver supports analyst tables."
  @callback supports_analyst_tables?() :: boolean

  @doc "Prepare analyst table for storing."
  @callback prepare_analyst_table(String.t(), Query.t()) :: String.t()

  @doc "Creates or updates the analyst table from the given data obtained via `prepare_analyst_table/2`."
  @callback create_or_update_analyst_table(
              connection,
              db_name :: String.t(),
              store_info :: String.t(),
              air_id :: String.t(),
              data_source_name :: String.t()
            ) :: :ok | {:error, String.t()}

  @doc "Given the list of known analyst tables, drops all existing but unused analyst tables."
  @callback drop_unused_analyst_tables(connection, known_db_names :: [String.t()]) :: removed :: [String.t()]

  @doc "Creates the analyst meta table in the database."
  @callback initialize_analyst_meta_table(connection) :: :ok | {:error, String.t()}

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def supports_analyst_tables?(), do: false

      @impl unquote(__MODULE__)
      def prepare_analyst_table(_table_name, _query), do: raise(RuntimeError, "not implemented")

      @impl unquote(__MODULE__)
      def create_or_update_analyst_table(_connection, _db_name, _store_info, _air_id, _data_source_name),
        do: raise(RuntimeError, "not implemented")

      @impl unquote(__MODULE__)
      def drop_unused_analyst_tables(_connection, _known_db_names), do: raise(RuntimeError, "not implemented")

      @impl unquote(__MODULE__)
      def initialize_analyst_meta_table(_connectcion), do: raise(RuntimeError, "not implemented")

      defoverridable unquote(__MODULE__)
    end
  end
end
