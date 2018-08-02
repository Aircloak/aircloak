defmodule Air.Service.LDAP.User do
  @enforce_keys [:dn, :login, :name]
  defstruct [:dn, :login, :name]
end
