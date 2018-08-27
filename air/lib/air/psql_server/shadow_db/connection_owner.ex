defmodule Air.PsqlServer.ShadowDb.ConnectionOwner do
  @moduledoc """
  Owner process of the database connection.

  This process manages a lifecycle of the database connection. The process lazily opens the connection on first access,
  and reopens the connection if it has since been closed. Finally, the owner process closes the connection if there has
  been no activity in a while.
  """
  use Parent.GenServer
  alias Air.PsqlServer.ShadowDb.Connection

  @idle_timeout :timer.minutes(1)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the given SQL query."
  @spec query(pid, String.t(), [term]) :: Connection.query_result()
  def query(pid, query, params) do
    conn = GenServer.call(pid, :connection, :timer.seconds(30))

    try do
      Connection.query(conn, query, params)
    after
      GenServer.cast(pid, :done)
    end
  end

  @doc "Parses the given SQL query."
  @spec parse(pid, String.t()) :: Connection.parse_result()
  def parse(pid, query) do
    conn = GenServer.call(pid, :connection, :timer.seconds(30))

    try do
      Connection.parse(conn, query)
    after
      GenServer.cast(pid, :done)
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(data_source_name), do: {:ok, data_source_name}

  @impl GenServer
  def handle_call(:connection, _from, data_source_name) do
    conn_pid =
      case Parent.GenServer.child_pid(Connection) do
        {:ok, pid} ->
          pid

        :error ->
          {:ok, pid} = Parent.GenServer.start_child({Connection, data_source_name})
          pid
      end

    {:reply, conn_pid, data_source_name}
  end

  @impl GenServer
  def handle_cast(:done, data_source_name), do: {:noreply, data_source_name, @idle_timeout}

  @impl GenServer
  def handle_info(:timeout, data_source_name) do
    if Parent.GenServer.child?(Connection), do: Parent.GenServer.shutdown_child(Connection)
    {:noreply, data_source_name}
  end

  def handle_info(other, data_source_name) do
    super(other, data_source_name)
    {:noreply, data_source_name, @idle_timeout}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(data_source_name), do: Parent.GenServer.start_link(__MODULE__, data_source_name)
end
