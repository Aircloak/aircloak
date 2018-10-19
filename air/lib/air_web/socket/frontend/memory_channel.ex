defmodule AirWeb.Socket.Frontend.MemoryChannel do
  @moduledoc "Channel used for communicating cloak memory to admin users."
  use Air.Web, :channel

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts the latest cloak memory readings."
  @spec broadcast_memory_readings() :: :ok
  def broadcast_memory_readings() do
    message = %{cloaks: Air.Service.Cloak.all_cloak_infos()}
    AirWeb.Endpoint.broadcast_from!(self(), "memory_readings", "updated_cloak_infos", message)
    :ok
  end

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("memory_readings", _, socket) do
    user = socket.assigns.user

    if Air.Schemas.User.admin?(user) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
