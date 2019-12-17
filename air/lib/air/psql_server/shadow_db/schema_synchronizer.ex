defmodule Air.PsqlServer.ShadowDb.SchemaSynchronizer do
  @moduledoc "Server responsible for synchronizing shadow dbs of users."
  use GenServer

  alias Air.PsqlServer.ShadowDb

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns whether events are being listened to."
  @spec events_enabled?() :: boolean()
  def events_enabled?(), do: GenServer.call(__MODULE__, :events_enabled?)

  @doc "Enables handlers to events for updating shadow dbs."
  @spec enable_events() :: :ok
  def enable_events(), do: GenServer.call(__MODULE__, :enable_events)

  @doc "Disables handlers to events for updating shadow dbs."
  @spec disable_events() :: :ok
  def disable_events(), do: GenServer.call(__MODULE__, :disable_events)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(listen) do
    subscribe()
    {:ok, listen}
  end

  @impl GenServer
  def handle_call(:events_enabled?, _, listening), do: {:reply, listening, listening}

  @impl GenServer
  def handle_call(:enable_events, _, _), do: {:reply, :ok, true}

  @impl GenServer
  def handle_call(:disable_events, _, _), do: {:reply, :ok, false}

  @impl GenServer
  def handle_info({:user_deleted, data}, true) do
    %{user: user, previous_data_sources: data_sources} = data

    data_sources
    |> Enum.each(&ShadowDb.drop(user, &1))

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:user_updated, data}, true) do
    %{user: user, previous_data_sources: previous_data_sources} = data

    current_data_sources =
      Air.Service.DataSource.for_user(user)
      |> Enum.map(& &1.name)

    update_shadow_db(user, previous_data_sources, current_data_sources)
    {:noreply, true}
  end

  def handle_info(_, listening), do: {:noreply, listening}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_shadow_db(user, datasources_before, datasources_after) do
    datasources_before
    |> Enum.reject(&Enum.member?(datasources_after, &1))
    |> Enum.each(&ShadowDb.drop(user, &1))

    datasources_after
    |> Enum.reject(&Enum.member?(datasources_before, &1))
    |> Enum.each(&ShadowDb.update(user, &1))
  end

  defp subscribe() do
    Air.Service.User.subscribe_to(:user_deleted)
    Air.Service.User.subscribe_to(:user_updated)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def start_link(listen \\ true) do
    GenServer.start_link(__MODULE__, listen, name: __MODULE__)
  end
end
