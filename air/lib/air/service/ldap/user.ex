defmodule Air.Service.LDAP.User do
  @enforce_keys [:dn, :name]
  defstruct [:dn, :name]
end
