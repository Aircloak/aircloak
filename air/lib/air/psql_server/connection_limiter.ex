defmodule Air.PsqlServer.ConnectionLimiter do
  @moduledoc """
  Application-level connection limiter.

  This process complements the connection limiting functionality of the ranch tcp server. The ranch connection limiter
  is soft, which means it can be exceeded. In addition, the ranch limiter is fairly naive. Any incomming connection
  which exceeds the limit will be placed into an indefinite waiting state.

  This limiter works at the application level, after the connection has been accepted. The limiter will refuse too many
  connections, and the client (`Air.PsqlServer.RanchServer`) will close the connection before performing the PostgreSQL
  handshake. In other words, this limiter enforces a hard-limit, and implements load-shedding.
  """
  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Registers the connection with the connection limiter.

  The function returns true if the connection has been successfully registered. In this case, the connection process
  is monitored by the limiter, and it will be automatically deregistered after it terminates.

  If the maximum number of the connections is open, the function will return false.
  """
  @spec register() :: boolean
  def register(), do: GenServer.call(__MODULE__, :register)

  # -------------------------------------------------------------------
  # GenServer callback functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init(max_connections), do: {:ok, %{connections: MapSet.new(), max_connections: max_connections}}

  @impl GenServer
  def handle_call(:register, {pid, _ref}, state) do
    if MapSet.size(state.connections) < state.max_connections do
      Process.monitor(pid)
      {:reply, true, %{state | connections: MapSet.put(state.connections, pid)}}
    else
      {:reply, false, state}
    end
  end

  @impl GenServer
  def handle_info({:DOWN, _mref, :process, pid, _reason}, state),
    do: {:noreply, %{state | connections: MapSet.delete(state.connections, pid)}}

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(max_connections) when is_integer(max_connections) and max_connections > 0,
    do: GenServer.start_link(__MODULE__, max_connections, name: __MODULE__)
end
