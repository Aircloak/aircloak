defmodule Cloak.DataSource.Driver do
  @moduledoc "Specifies the interface for implementing the database specific data access operations."

  alias Cloak.Sql.Query
  alias Cloak.DataSource.Table

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
end
