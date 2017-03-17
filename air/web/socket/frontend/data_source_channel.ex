defmodule Air.Socket.Frontend.DataSourceChannel do
  @moduledoc "Channel used for communicating data source availability to users."
  use Air.Web, :channel


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts an online or offline message for each known data source."
  @spec push_updates() :: :ok
  def push_updates() do
    for %{id: id, global_id: global_id} <- Air.Service.DataSource.all() do
      status = case Air.Service.DataSource.available?(global_id) do
        true -> "online"
        false -> "offline"
      end
      Air.Endpoint.broadcast!("data_source:#{id}", status, %{})
    end

    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("data_source:" <> id, _, socket) do
    case Air.Service.DataSource.fetch_as_user({:id, id}, socket.assigns.user) do
      {:ok, _} -> {:ok, socket}
      {:error, :unauthorized} -> {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
