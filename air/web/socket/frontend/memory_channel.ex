defmodule Air.Socket.Frontend.MemoryChannel do
  @moduledoc "Channel used for communicating cloak memory to admin users."
  use Air.Web, :channel


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts the latest cloak memory readings."
  @spec broadcast_memory_reading(Map.t) :: :ok
  def broadcast_memory_reading(memory_reading) do
    Air.Endpoint.broadcast_from!(self(), "memory_readings", "new_reading", memory_reading)
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("memory_readings", _, socket) do
    user = socket.assigns.user
    if Air.Schemas.User.admin?(user) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
