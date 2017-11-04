defmodule AirWeb.Socket.Frontend.MemoryChannel do
  @moduledoc "Channel used for communicating cloak memory to admin users."
  use Air.Web, :channel


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts the latest cloak memory readings."
  @spec broadcast_memory_reading(String.t, Map.t) :: :ok
  def broadcast_memory_reading(cloak_id, memory_reading) do
    for cloak_info <- Air.Service.Cloak.all_cloak_infos(), cloak_info[:id] == cloak_id do
      formatted_cloak = AirWeb.Admin.ActivityMonitorView.format_cloak(cloak_info, memory_reading)
      AirWeb.Endpoint.broadcast_from!(self(), "memory_readings", "new_reading", formatted_cloak)
    end
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
