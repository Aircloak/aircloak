defmodule Air.PsqlServer.ShadowDb.SchemaSynchronizer do
  @moduledoc "Server responsible for synchronizing shadow dbs of users."
  use GenServer

  alias Air.PsqlServer.ShadowDb
  alias Air.Repo

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Waits for the schema synchronizer to complete existing work."
  @spec flush :: :ok
  def flush(), do: GenServer.call(__MODULE__, :flush)

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
  def handle_call(:flush, _, listening), do: {:reply, :ok, listening}

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
  def handle_info({:group_deleted, data}, true) do
    sync_group_users(data.previous_users_and_data_sources)
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
      |> Enum.each(&ShadowDb.update(user, &1.name))
    end

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:data_source_deleted, data}, true) do
    %{data_source_name: data_source_name, previous_users: previous_users} = data

    previous_users
    |> Enum.each(&ShadowDb.drop(&1, data_source_name))

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:data_source_updated, data}, true) do
    %{data_source_name: data_source_name, previous_users: previous_users} = data
    data_source = Air.Service.DataSource.by_name(data_source_name)

    new_users =
      Repo.preload(data_source, groups: :users).groups
      |> Stream.flat_map(& &1.users)
      |> MapSet.new()

    revoked_users = MapSet.difference(previous_users, new_users)

    new_users
    |> Enum.each(&ShadowDb.update(&1, data_source_name))

    revoked_users
    |> Enum.each(&ShadowDb.drop(&1, data_source_name))

    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:revalidated_views, data}, true) do
    %{user_id: user_id, data_source_id: data_source_id} = data
    sync_by_ids(user_id, data_source_id)
    {:noreply, true}
  end

  @impl GenServer
  def handle_info({:revalidated_analyst_tables, data}, true) do
    %{user_id: user_id, data_source_id: data_source_id} = data
    sync_by_ids(user_id, data_source_id)
    {:noreply, true}
  end

  def handle_info({:data_sources_registered, data}, true) do
    %{data_sources: data_sources} = data

    data_sources
    |> Enum.map(&Air.Service.DataSource.by_name(&1))
    |> Enum.each(&update_data_source(&1))

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
    Air.Service.DataSource.subscribe_to(:data_source_updated)
    Air.Service.DataSource.subscribe_to(:data_source_deleted)
    Air.Service.View.subscribe_to(:revalidated_views)
    Air.Service.Cloak.subscribe_to(:data_sources_registered)
    Air.Service.AnalystTable.subscribe_to(:revalidated_analyst_tables)
  end

  defp update_shadow_db(user, data_sources_before, data_sources_after) do
    data_sources_before
    |> Enum.reject(&Enum.member?(data_sources_after, &1))
    |> Enum.each(&ShadowDb.drop(user, &1))

    data_sources_after
    |> Enum.reject(&Enum.member?(data_sources_before, &1))
    |> Enum.each(&ShadowDb.update(user, &1))
  end

  defp update_data_source(data_source),
    do:
      data_source
      |> Air.Service.DataSource.users()
      |> Enum.each(&ShadowDb.update(&1, data_source.name))

  defp sync_group_users(previous_data_sources) do
    for {user, data_sources_before} <- previous_data_sources do
      data_sources_after = data_source_names(user)
      update_shadow_db(user, data_sources_before, data_sources_after)
    end
  end

  defp sync_by_ids(user_id, data_source_id) do
    user = Air.Service.User.load!(user_id)
    data_source = Air.Service.DataSource.by_id!(data_source_id)
    ShadowDb.update(user, data_source.name)
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
