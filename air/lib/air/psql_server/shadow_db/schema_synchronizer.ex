defmodule Air.PsqlServer.ShadowDb.SchemaSynchronizer do
  @moduledoc "Server responsible for synchronizing shadow dbs of users."
  use GenServer

  alias Air.PsqlServer.ShadowDb
  alias Air.Repo

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
    current_data_sources = data_source_names(user)

    update_shadow_db(user, previous_data_sources, current_data_sources)

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:group_updated, data}, true) do
    %{group: group, previous_users_and_data_sources: previous_state} = data

    sync_group_users(previous_state)

    group = Repo.preload(group, :users)

    for user <- group.users, not Map.has_key?(previous_state, user) do
      # We do not have a handle on which data sources this user already had access to
      # prior to joining this group. They could all be a result of joining the group.
      Air.Service.DataSource.for_user(user)
      |> Enum.each(&Air.PsqlServer.ShadowDb.update(user, &1.name))
    end

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:group_deleted, data}, true) do
    sync_group_users(data.previous_users_and_data_sources)
    {:noreply, true}
  end

  def handle_info(_, listening), do: {:noreply, listening}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp subscribe() do
    Air.Service.User.subscribe_to(:user_deleted)
    Air.Service.User.subscribe_to(:user_updated)
    Air.Service.Group.subscribe_to(:group_updated)
    Air.Service.Group.subscribe_to(:group_deleted)
  end

  defp update_shadow_db(user, data_sources_before, data_sources_after) do
    data_sources_before
    |> Enum.reject(&Enum.member?(data_sources_after, &1))
    |> Enum.each(&ShadowDb.drop(user, &1))

    data_sources_after
    |> Enum.reject(&Enum.member?(data_sources_before, &1))
    |> Enum.each(&ShadowDb.update(user, &1))
  end

  defp sync_group_users(previous_data_sources) do
    for {user, data_sources_before} <- previous_data_sources do
      data_sources_after = data_source_names(user)
      update_shadow_db(user, data_sources_before, data_sources_after)
    end
  end

  defp data_source_names(user) do
    Air.Service.DataSource.for_user(user)
    |> Enum.map(& &1.name)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def start_link(listen \\ true) do
    GenServer.start_link(__MODULE__, listen, name: __MODULE__)
  end
end
