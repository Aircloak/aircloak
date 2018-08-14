defmodule Air.Service.LDAP.Normalization do
  @moduledoc """
  Deals with normalizing data obtained from LDAP so that the syncing process afterwards can always behave in the same
  way.
  """

  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Normalizes the following aspects of groups obtained from LDAP:

  * Moves any membership information from users to groups
  * Normalizes membership information stored by DN to logins
  """
  @spec normalize_groups({:ok, map()} | :error, [User.t()], [Group.t()]) :: [Group.t()]
  def normalize_groups(config \\ Aircloak.DeployConfig.fetch("ldap"), users, groups)

  def normalize_groups(:error, _, groups), do: groups

  def normalize_groups({:ok, config}, users, groups) do
    groups
    |> Enum.map(&normalize_member_key(&1, users, config))
    |> Enum.map(&normalize_members_from_users(&1, users))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_member_key(group, users, %{"group_member_key" => "dn"}) do
    member_ids =
      group.member_ids
      |> Enum.map(&login_for_dn(&1, users))
      |> Enum.reject(&is_nil/1)

    %{group | member_ids: member_ids}
  end

  defp normalize_member_key(group, _, _), do: group

  defp login_for_dn(dn, users) do
    case Enum.find(users, &(&1.dn == dn)) do
      nil -> nil
      user -> user.login
    end
  end

  defp normalize_members_from_users(group, users) do
    update_in(group, [Lens.key(:member_ids)], &Enum.uniq(members_from_users(group.dn, users) ++ &1))
  end

  defp members_from_users(dn, users) do
    users
    |> Enum.filter(&(dn in &1.group_dns))
    |> Enum.map(& &1.login)
  end
end
