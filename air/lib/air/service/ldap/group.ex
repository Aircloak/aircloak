defmodule Air.Service.LDAP.Group do
  @enforce_keys [:dn, :name, :member_dns]
  defstruct [:dn, :name, :member_dns]
end
