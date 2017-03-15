defmodule Air.Socket.Frontend.DataSourceChannel do
  @moduledoc "Channel used for communicating data source availability to users."
  use Air.Web, :channel


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("data_source:" <> global_id, _, socket) do
    case Air.Service.DataSource.fetch_as_user({:global_id, global_id}, socket.assigns.user) do
      {:ok, data_source} -> {:ok, socket}
      {:error, :unauthorized} -> {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
