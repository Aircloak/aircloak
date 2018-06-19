defmodule AirWeb.Socket.Frontend.DataSourceChannel do
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
      AirWeb.Endpoint.broadcast!("data_source:#{data_source.name}", "status", %{
        status: DataSource.status(data_source)
      })
    end

    :ok
  end

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("data_source:" <> name, _, socket) do
    case DataSource.fetch_as_user({:name, name}, socket.assigns.user) do
      {:ok, _} ->
        {:ok, socket}

      {:error, :unauthorized} ->
        {:error, %{success: false, description: "Unauthorized to access channel"}}
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Periodic.child_spec(run: &push_updates/0, every: :timer.seconds(10))
end
