defmodule Air.Service.LDAP.Sync do
  def sync(users, _groups) do
    for user <- users do
      {:ok, _} =
        Air.Service.User.create_ldap(%{
          login: user.login,
          ldap_dn: user.dn,
          name: user.name
        })
    end
  end
end
