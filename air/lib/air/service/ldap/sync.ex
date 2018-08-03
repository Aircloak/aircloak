defmodule Air.Service.LDAP.Sync do
  def sync(users, _groups) do
    Enum.each(users, &sync_user/1)
  end

  defp sync_user(user) do
    cond do
      Air.Repo.get_by(Air.Schemas.User, login: user.login) -> :ignore
      true -> create_user!(user)
    end
  end

  defp create_user!(user) do
    {:ok, _} =
      Air.Service.User.create_ldap(%{
        login: user.login,
        ldap_dn: user.dn,
        name: user.name
      })
  end
end
