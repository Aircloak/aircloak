defmodule Air.Settings do
  @moduledoc "Represents a set of Air-wide options, set by the admin."

  @type t :: %__MODULE__{
          query_retention_days: integer | :undefined,
          audit_log_enabled: boolean,
          decimal_sep: String.t(),
          thousand_sep: String.t(),
          decimal_digits: integer,
          ldap_host: String.t(),
          ldap_port: integer,
          ldap_ssl: boolean,
          ldap_ca_cert: String.t()
        }

  defstruct [
    :query_retention_days,
    :audit_log_enabled,
    :decimal_sep,
    :thousand_sep,
    :decimal_digits,
    :ldap_host,
    :ldap_port,
    :ldap_ssl,
    :ldap_ca_cert
  ]
end
