defmodule Air.Service.LDAP.Sync do
  import Ecto.Query

  def sync(ldap_users, ldap_groups) do
    user_mappings = sync_users(ldap_users)
    sync_groups(ldap_groups, user_mappings)
    disable_missing_users(ldap_users)
    delete_missing_groups(ldap_groups)
  end

  defp sync_groups(ldap_groups, user_mappings), do: Enum.each(ldap_groups, &sync_group(&1, user_mappings))

  defp sync_group(ldap_group, user_mappings) do
    case Air.Repo.get_by(Air.Schemas.Group, ldap_dn: ldap_group.dn) do
      nil -> create_group!(ldap_group, user_mappings)
      air_group -> update_group!(air_group, ldap_group)
    end
  end

  defp create_group!(ldap_group, user_mappings) do
    {:ok, _} =
      Air.Service.User.create_ldap_group(%{
        admin: false,
        name: ldap_group.name,
        ldap_dn: ldap_group.dn,
        users: Enum.map(ldap_group.member_ids, &Map.get(user_mappings, &1))
      })
  end

  defp update_group!(air_group, ldap_group) do
    Air.Service.User.update_group!(air_group, %{admin: false, name: ldap_group.name}, ldap: true)
  end

  defp sync_users(ldap_users), do: ldap_users |> Enum.flat_map(&sync_user/1) |> Enum.into(%{})

  defp sync_user(ldap_user) do
    cond do
      Air.Repo.get_by(Air.Schemas.User, login: ldap_user.login) ->
        []

      air_user = Air.Repo.get_by(Air.Schemas.User, ldap_dn: ldap_user.dn) ->
        [{ldap_user.login, update_user!(air_user, ldap_user).id}]

      true ->
        [{ldap_user.login, create_user!(ldap_user).id}]
    end
  end

  defp create_user!(ldap_user) do
    {:ok, user} =
      Air.Service.User.create_ldap(%{
        login: ldap_user.login,
        ldap_dn: ldap_user.dn,
        name: ldap_user.name
      })

    user
  end

  defp update_user!(air_user, ldap_user) do
    air_user
    |> Air.Service.User.update!(%{login: ldap_user.login, name: ldap_user.name}, ldap: true)
    |> Air.Service.User.enable!(ldap: true)
  end

  defp disable_missing_users(ldap_users) do
    present_dns = Enum.map(ldap_users, & &1.dn)

    Air.Schemas.User
    |> where([q], q.source == ^:ldap)
    |> where([q], not (q.ldap_dn in ^present_dns))
    |> Air.Repo.all()
    |> Enum.each(&Air.Service.User.disable(&1, ldap: true))
  end

  defp delete_missing_groups(ldap_groups) do
    present_dns = Enum.map(ldap_groups, & &1.dn)

    Air.Schemas.Group
    |> where([q], q.source == ^:ldap)
    |> where([q], not (q.ldap_dn in ^present_dns))
    |> Air.Repo.all()
    |> Enum.each(&Air.Service.User.delete_group!(&1, ldap: true))
  end
end
