defmodule Cloak.DataSource.Driver do
  @moduledoc "Specifies the interface for implementing the database specific data access operations."

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.Table

  @doc "Returns the configured database connection timeout."
  @spec connect_timeout() :: pos_integer
  def connect_timeout(), do:
    Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:connect_timeout)

  @doc "Returns the configured time for keeping the connection alive for potential reuse."
  @spec connection_keep_time() :: pos_integer
  def connection_keep_time(), do:
    Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:connection_keep_time)

  @doc "Returns the configured maximum timeout for a database operation."
  @spec timeout() :: pos_integer
  def timeout(), do:
    Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:timeout)

  @doc "Returns the configured batch size for a select operation."
  @spec batch_size() :: pos_integer
  def batch_size(), do:
    Application.get_env(:cloak, :data_source) |> Keyword.fetch!(:batch_size)

  @type connection :: any
  @type parameters :: any

  @doc "Returns an atom describing the query dialect of this specific driver implementation."
  @callback sql_dialect_module(parameters) :: module | nil

  @doc "Opens a new connection to the data store."
  @callback connect!(parameters) :: connection

  @doc "Closes the connection to the data store."
  @callback disconnect(connection) :: :ok

  @doc "Loads one or more table definitions from the data store."
  @callback load_tables(connection, Table.t) :: [Table.t]

  @doc "Driver specific implementation for the `DataSource.select` functionality."
  @callback select(connection, Query.t, Cloak.DataSource.result_processor)
    :: {:ok, Cloak.DataSource.processed_result} | {:error, any}

  @doc "Checks to see if the driver is able to handle all the SQL features used by the query."
  @callback supports_query?(Query.t) :: boolean

  @doc "Checks to see if the driver is able to handle specified function natively."
  @callback supports_function?(Expression.t, Cloak.DataSource.t) :: boolean

  @doc "Returns true if the connection can be used from processes other than the creator process."
  @callback supports_connection_sharing?() :: boolean
end
