defmodule Air.Service.LDAP.Normalization do
  def normalize(users, groups) do
    groups =
      Enum.map(groups, fn group ->
        update_in(group, [Lens.key(:member_ids)], &Enum.uniq(members_from_users(group.dn, users) ++ &1))
      end)

    {users, groups}
  end

  defp members_from_users(dn, users) do
    users
    |> Enum.filter(&(dn in &1.group_dns))
    |> Enum.map(& &1.login)
  end
end
