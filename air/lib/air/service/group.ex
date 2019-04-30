defmodule Air.Service.Group do
  alias Air.Repo
  alias Air.Schemas.{Group, User, DataSource}

  import Ecto.Query
  import Ecto.Changeset

  @ldap_fields ~w(ldap_dn source)a
  @ldap_required_fields ~w(ldap_dn)a

  @type change_options :: [ldap: true | false | :any]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new group, raises on error."
  @spec create_group!(map) :: Group.t()
  def create_group!(params) do
    {:ok, group} = create_group(params)
    group
  end

  @doc "Creates the new group from the given parameters."
  @spec create_group(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_group(params),
    do:
      %Group{}
      |> group_changeset(params)
      |> Repo.insert()

  @doc "Creates a new LDAP group."
  @spec create_ldap_group(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap_group(params) do
    %Group{}
    |> group_changeset(params, ldap: true)
    |> merge(ldap_changeset(%Group{}, params))
    |> Repo.insert()
  end

  @doc "Updates the given group, raises on error."
  @spec update_group!(Group.t(), map, change_options) :: Group.t()
  def update_group!(group, params, options \\ []) do
    {:ok, group} = update_group(group, params, options)
    group
  end

  @doc "Updates the given group."
  @spec update_group(Group.t(), map, change_options) ::
          {:ok, Group.t()} | {:error, Ecto.Changeset.t() | :forbidden_no_active_admin}
  def update_group(group, params, options \\ []) do
    check_ldap!(group, options)

    Air.Service.User.commit_if_active_last_admin(fn ->
      group
      |> group_changeset(params, options)
      |> Repo.update()
    end)
  end

  @doc "Updates only the data sources of the given group."
  @spec update_group_data_sources(Group.t(), map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def update_group_data_sources(group, params) do
    group
    |> group_data_source_changeset(params)
    |> Repo.update()
  end

  @doc "Deletes the given group, raises on error."
  @spec delete_group!(Group.t(), change_options) :: Group.t()
  def delete_group!(group, options \\ []) do
    {:ok, group} = delete_group(group, options)
    group
  end

  @doc "Deletes the given group."
  @spec delete_group(Group.t(), change_options) :: {:ok, Group.t()} | {:error, :forbidden_no_active_admin}
  def delete_group(group, options \\ []) do
    check_ldap!(group, options)
    Air.Service.User.commit_if_active_last_admin(fn -> Repo.delete(group) end)
  end

  @doc "Loads the group with the given id."
  @spec load_group(pos_integer) :: Group.t() | nil
  def load_group(group_id),
    do: Repo.one(from(group in Group, where: group.id == ^group_id, preload: [:users, :data_sources]))

  @doc "Returns a list of all groups in the system."
  @spec all_groups() :: [Group.t()]
  def all_groups(), do: Repo.all(from(group in Group, preload: [:users, :data_sources]))

  @doc "Returns the empty changeset for the new group."
  @spec empty_group_changeset() :: Ecto.Changeset.t()
  def empty_group_changeset(), do: group_changeset(%Group{}, %{})

  @doc "Converts a group into a changeset."
  @spec group_to_changeset(Group.t()) :: Ecto.Changeset.t()
  def group_to_changeset(group), do: group_changeset(group, %{})

  @doc "Returns all admin groups."
  @spec admin_groups() :: [Group.t()]
  def admin_groups(), do: Repo.all(from(g in Group, where: g.admin))

  @doc "Returns a group by name"
  @spec get_group_by_name(String.t()) :: {:ok, Group.t()} | {:error, :not_found}
  def get_group_by_name(name) do
    case(Air.Repo.get_by(Air.Schemas.Group, name: name)) do
      nil -> {:error, :not_found}
      group -> {:ok, group}
    end
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp group_changeset(group, params, options \\ []),
    do:
      group
      |> cast(params, ~w(name admin)a)
      |> validate_required(~w(name admin)a)
      |> unique_constraint(:name, name: :groups_name_source_index)
      |> PhoenixMTM.Changeset.cast_collection(:users, Repo, User)
      |> validate_change(:users, &validate_group_user_source(&1, &2, options))
      |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)

  defp validate_group_user_source(:users, users, options) do
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

  defp group_data_source_changeset(group, params) do
    group
    |> cast(params, [])
    |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)
  end
end
