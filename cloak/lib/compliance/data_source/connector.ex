defmodule Compliance.DataSource.Connector do
  @moduledoc "Specifies the interface needed to create tables and insert data for a test dataset"

  @type state :: any

  @doc "Setup up the compliance database."
  @callback setup(Cloak.DataSource.t()) :: :ok

  @doc "Establishes a connection to the database."
  @callback connect(Cloak.DataSource.t()) :: state

  @doc "Is expected to create the necessary tables for the test dataset."
  @callback create_table(
              String.t(),
              [{String.t(), Compliance.TableDefinitions.column_type()}],
              state
            ) :: state

  @doc "Is expected to insert the provided data into the data source."
  @callback insert_rows(String.t(), [Map.t()], state) :: state

  @doc "Is expected to insert the provided documents into the data source."
  @callback insert_documents(String.t(), [Map.t()], state) :: state

  @doc "Invoked after all the tables have been created."
  @callback after_tables_created(state) :: state

  @doc "Is supposed to do any cleanup if required."
  @callback terminate(state) :: :ok

  @doc """
  Is supposed to return the database table name from the given table name.

  This can be useful if a table name needs to be additionally decorated with some prefix.
  """
  @callback db_table_name(String.t()) :: String.t()

  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def db_table_name(table_name), do: table_name

      @impl unquote(__MODULE__)
      def insert_documents(_collection_name, _documents, conn), do: conn

      defoverridable db_table_name: 1, insert_documents: 3
    end
  end

  @doc "Waits for the port on the given host to become available."
  @spec await_port(String.t(), :inet.port_number()) :: :ok
  def await_port(host, port) do
    IO.puts("waiting for #{host}:#{port}...")

    task =
      Task.async(fn ->
        fn -> :gen_tcp.connect(to_charlist(host), port, []) end
        |> Stream.repeatedly()
        |> Stream.drop_while(&match?({:error, _}, &1))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(2)) do
      nil -> Mix.raise("#{host}:#{port} is not available")
      _ -> IO.puts("#{host}:#{port} is available")
    end
  end
end
