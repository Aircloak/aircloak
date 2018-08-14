defmodule Air.Service.LDAP.Normalization do
  require Aircloak.DeployConfig

  def normalize_groups(config \\ Aircloak.DeployConfig.fetch("ldap"), users, groups)

  def normalize_groups(:error, _, groups), do: groups

  def normalize_groups({:ok, config}, users, groups) do
    groups
    |> Enum.map(&normalize_member_key(&1, users, config))
    |> Enum.map(&normalize_members_from_users(&1, users))
  end

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
