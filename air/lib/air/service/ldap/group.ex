defmodule Air.Service.LDAP.Group do
  @enforce_keys [:dn, :name, :member_ids]
  defstruct [:dn, :name, :member_ids]
end
