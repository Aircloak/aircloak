defmodule Air.Service.ShadowDb.Server do
  @moduledoc "Server responsible for managing a single shadow database."

  use GenServer
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Updates the data source definition."
  @spec update_definition(pid, map) :: :ok
  def update_definition(server, data_source), do: GenServer.cast(server, {:update, data_source})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, %{data_source: nil}}

  @impl GenServer
  def handle_cast({:update, data_source}, state) do
    if state.data_source != data_source, do: update_shadow_db(data_source)
    {:noreply, %{state | data_source: data_source}}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_shadow_db(data_source) do
    Logger.info("data source definition changed for #{data_source.name}")
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(registered_name), do: GenServer.start_link(__MODULE__, nil, name: registered_name)
end
