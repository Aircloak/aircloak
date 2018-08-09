defmodule Air.Service.LDAP.Group do
  @moduledoc "A struct representing a group obtained from LDAP."

  @type t :: %__MODULE__{
          dn: String.t(),
          name: String.t(),
          member_ids: [String.t()]
        }

  @enforce_keys [:dn, :name, :member_ids]
  defstruct [:dn, :name, :member_ids]
end
