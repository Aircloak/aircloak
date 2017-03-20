defmodule Air.Socket.Frontend.DataSourceChannel do
  @moduledoc "Channel used for communicating data source availability to users."
  use Air.Web, :channel

  alias Air.Service.DataSource


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Broadcasts an online or offline message for each known data source."
  @spec push_updates() :: :ok
  def push_updates() do
    for data_source <- DataSource.all() do
      Air.Endpoint.broadcast!("data_source:#{data_source.id}", "status", %{status: DataSource.status(data_source)})
    end

    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("data_source:" <> id, _, socket) do
    case DataSource.fetch_as_user({:id, id}, socket.assigns.user) do
      {:ok, _} -> {:ok, socket}
      {:error, :unauthorized} -> {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end
end
