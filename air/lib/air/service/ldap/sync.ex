defmodule Air.Service.LDAP.Sync do
  def sync(users, _groups) do
    Enum.each(users, &sync_user/1)
  end

  defp sync_user(ldap_user) do
    cond do
      Air.Repo.get_by(Air.Schemas.User, login: ldap_user.login) -> :ignore
      air_user = Air.Repo.get_by(Air.Schemas.User, ldap_dn: ldap_user.dn) -> update_user!(air_user, ldap_user)
      true -> create_user!(ldap_user)
    end
  end

  defp create_user!(ldap_user) do
    {:ok, _} =
      Air.Service.User.create_ldap(%{
        login: ldap_user.login,
        ldap_dn: ldap_user.dn,
        name: ldap_user.name
      })
  end

  defp update_user!(air_user, ldap_user) do
    {:ok, _} = Air.Service.User.update_ldap(air_user, %{login: ldap_user.login, name: ldap_user.name})
  end
end
