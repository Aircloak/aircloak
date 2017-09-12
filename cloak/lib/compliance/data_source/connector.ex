defmodule Compliance.DataSource.Connector do
  @moduledoc "Specifies the interface needed to create tables and insert data for a test dataset"

  @type state :: any

  @doc "Establishes a connection to the database and does other necessary setup"
  @callback setup(Cloak.DataSource.t) :: state

  @doc "Is expected to create the necessary tables for the test dataset"
  @callback create_table(String.t, [{String.t, Compliance.TableDefinitions.column_type}], state) :: state

  @doc "Is expected to insert the provided data into the data source"
  @callback insert_rows(String.t, [Map.t], state) :: state

  @doc "Is expected to insert the provided documents into the data source"
  @callback insert_documents(String.t, [Map.t], state) :: state

  @doc "Is supposed to do any cleanup if required"
  @callback terminate(state) :: :ok

  @doc """
  Is supposed to return the database table name from the given table name.

  This can be useful if a table name needs to be additionally decorated with some prefix.
  """
  @callback db_table_name(String.t) :: String.t

  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      @doc false
      def db_table_name(table_name), do: table_name

      @doc false
      def insert_documents(_collection_name, _documents, conn), do: conn

      defoverridable db_table_name: 1, insert_documents: 3
    end
  end
end
