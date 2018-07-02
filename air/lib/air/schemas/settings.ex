defmodule Air.Schemas.Settings do
  @moduledoc "Schema for serializing Air.Settings into the DB."

  use Air.Schemas.Base

  schema "settings" do
    field(:query_retention_days, :integer)
    field(:audit_log_enabled, :boolean)

    # number format settings
    field(:decimal_sep, :string)
    field(:thousand_sep, :string)
    field(:decimal_digits, :integer)

    # LDAP settings
    field(:ldap_host, :string)
    field(:ldap_port, :integer)
    field(:ldap_ssl, :boolean)
    field(:ldap_ca_cert, :string)

    timestamps()
  end
end
