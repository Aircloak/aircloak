defmodule AirWeb.Socket.Frontend.CloakStatsChannel do
  @moduledoc "Channel used for communicating cloak memory to admin users."
  use Air.Web, :channel

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts the latest cloak memory readings."
  @spec broadcast_cloak_stats([Map.t()]) :: :ok
  def broadcast_cloak_stats(stats) do
    AirWeb.Endpoint.broadcast_from!(self(), "cloak_stats", "updated_cloak_infos", %{cloaks: stats})
    :ok
  end

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("cloak_stats", _, socket) do
    user = socket.assigns.user

    if Air.Schemas.User.admin?(user) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
