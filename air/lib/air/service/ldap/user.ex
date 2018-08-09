defmodule Air.Service.LDAP.User do
  @moduledoc "A struct representing a user obtained from LDAP."

  @type t :: %__MODULE__{
          dn: String.t(),
          login: String.t(),
          name: String.t()
        }

  @enforce_keys [:dn, :login, :name]
  defstruct [:dn, :login, :name]
end
