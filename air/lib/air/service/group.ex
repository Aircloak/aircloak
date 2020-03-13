defmodule Air.Service.Group do
  @moduledoc "Service module for working with groups."

  alias Air.Repo
  alias Air.Schemas.{DataSource, Group, User}
  alias Air.Service.AdminGuard
  alias Aircloak.ChildSpec

  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset

  @ldap_fields ~w(ldap_dn source)a
  @ldap_required_fields ~w(ldap_dn)a

  @type change_options :: [ldap: true | false | :any]
  @type group_notification :: :group_updated | :group_deleted

  @notifications_registry __MODULE__.NotificationsRegistry

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Subscribes to notifications about activities."
  @spec subscribe_to(group_notification) :: :ok
  def subscribe_to(notification) do
    Registry.register(@notifications_registry, notification, nil)
    :ok
  end

  @doc "Subscribes to notifications about activities."
  @spec unsubscribe_from(group_notification) :: :ok
  def unsubscribe_from(notification) do
    Registry.unregister(@notifications_registry, notification)
    :ok
  end

  @doc "Creates the new group, raises on error."
  @spec create!(map) :: Group.t()
  def create!(params) do
    {:ok, group} = create(params)
    group
  end

  @doc "Creates the new group from the given parameters."
  @spec create(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create(params),
    do:
      %Group{}
      |> group_changeset(params)
      |> Repo.insert()

  @doc "Creates a new LDAP group."
  @spec create_ldap(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap(params) do
    %Group{}
    |> group_changeset(params, ldap: true)
    |> merge(ldap_changeset(%Group{}, params))
    |> Repo.insert()
  end

  @doc "Updates the given group, raises on error."
  @spec update!(Group.t(), map, change_options) :: Group.t()
  def update!(group, params, options \\ []) do
    {:ok, group} = update(group, params, options)
    group
  end

  @doc "Updates the given group."
  @spec update(Group.t(), map, change_options) ::
          {:ok, Group.t()} | {:error, Ecto.Changeset.t() | :forbidden_no_active_admin}
  def update(group, params, options \\ []) do
    check_ldap!(group, options)

    with_notification(
      group,
      :group_updated,
      fn ->
        AdminGuard.commit_if_active_last_admin(fn ->
          group
          |> group_changeset(params, options)
          |> Repo.update()
        end)
      end
    )
  end

  @doc "Updates only the data sources of the given group."
  @spec update_data_sources(Group.t(), map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def update_data_sources(group, params) do
    with_notification(
      group,
      :group_updated,
      fn ->
        group
        |> data_source_changeset(params)
        |> Repo.update()
      end
    )
  end

  @doc "Deletes the given group, raises on error."
  @spec delete!(Group.t(), change_options) :: Group.t()
  def delete!(group, options \\ []) do
    {:ok, group} = delete(group, options)
    group
  end

  @doc "Deletes the given group."
  @spec delete(Group.t(), change_options) :: {:ok, Group.t()} | {:error, :forbidden_no_active_admin}
  def delete(group, options \\ []) do
    check_ldap!(group, options)

    with_notification(
      group,
      :group_deleted,
      fn -> AdminGuard.commit_if_active_last_admin(fn -> Repo.delete(group) end) end
    )
  end

  @doc "Loads the group with the given id."
  @spec load(pos_integer) :: Group.t() | nil
  def load(group_id),
    do: Repo.one(from(group in Group, where: group.id == ^group_id, preload: [:users, :data_sources]))

  @doc "Returns a list of all groups in the system."
  @spec all() :: [Group.t()]
  def all(), do: Repo.all(from(group in Group, preload: [:users, :data_sources]))

  @doc "Returns the empty changeset for the new group."
  @spec empty_changeset() :: Ecto.Changeset.t()
  def empty_changeset(), do: group_changeset(%Group{}, %{})

  @doc "Converts a group into a changeset."
  @spec to_changeset(Group.t()) :: Ecto.Changeset.t()
  def to_changeset(group), do: group_changeset(group, %{})

  @doc "Returns all admin groups."
  @spec admin_groups() :: [Group.t()]
  def admin_groups(), do: Repo.all(from(g in Group, where: g.admin))

  @doc "Returns a group by name"
  @spec get_by_name(String.t()) :: {:ok, Group.t()} | {:error, :not_found}
  def get_by_name(name) do
    case(Air.Repo.get_by(Air.Schemas.Group, name: name)) do
      nil -> {:error, :not_found}
      group -> {:ok, group}
    end
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp notify_subscribers(notification, payload),
    do:
      Registry.lookup(@notifications_registry, notification)
      |> Enum.map(fn {pid, nil} -> pid end)
      |> Enum.each(&send(&1, {notification, payload}))

  defp group_changeset(group, params, options \\ []),
    do:
      group
      |> cast(params, ~w(name admin)a)
      |> validate_required(~w(name admin)a)
      |> unique_constraint(:name, name: :groups_name_source_index)
      |> PhoenixMTM.Changeset.cast_collection(:users, Repo, User)
      |> validate_change(:users, &validate_user_source(&1, &2, options))
      |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)

  defp validate_user_source(:users, users, options) do
    valid_source = if(Keyword.get(options, :ldap, false), do: :ldap, else: :native)
    invalid_users = Enum.filter(users, &(&1.data.source != valid_source))

    case {valid_source, invalid_users} do
      {_, []} -> []
      {:native, _} -> [users: "cannot assign LDAP users to a native group"]
      {:ldap, _} -> [users: "cannot assign native users to an LDAP group"]
    end
  end

  defp ldap_changeset(user, params) do
    user
    |> cast(Map.put(params, :source, :ldap), @ldap_fields)
    |> validate_required(@ldap_required_fields)
  end

  defp check_ldap!(group, options) do
    case {group.source, Keyword.get(options, :ldap, false)} do
      {_, :any} -> group
      {:ldap, true} -> group
      {:ldap, false} -> raise "Accidental LDAP change"
      {_, true} -> raise "Accidental non-LDAP change"
      _ -> group
    end
  end

  defp data_source_changeset(group, params) do
    group
    |> cast(params, [])
    |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)
  end

  defp with_notification(group, notification_type, alteration_function) do
    group = group |> Repo.preload(:users)

    users_and_data_sources_before =
      group.users
      |> Enum.map(fn user ->
        data_sources =
          Air.Service.DataSource.for_user(user)
          |> Enum.map(& &1.name)

        {user, data_sources}
      end)
      |> Enum.into(%{})

    case alteration_function.() do
      {:ok, altered_group} = result ->
        notify_subscribers(notification_type, %{
          group: altered_group,
          previous_users_and_data_sources: users_and_data_sources_before
        })

        result

      other ->
        other
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        ChildSpec.registry(:duplicate, @notifications_registry)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end
