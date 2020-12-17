defmodule Air.Settings do
  @moduledoc "Represents a set of Air-wide options, set by the admin."

  @type t :: %__MODULE__{
          query_retention_days: integer | :undefined,
          audit_log_enabled: boolean,
          decimal_sep: String.t(),
          thousand_sep: String.t(),
          decimal_digits: integer,
          login_message: String.t(),
          main_message: String.t(),
          type_checking_enabled: boolean
        }

  defstruct [
    :query_retention_days,
    :audit_log_enabled,
    :decimal_sep,
    :thousand_sep,
    :decimal_digits,
    :login_message,
    :main_message,
    :type_checking_enabled
  ]
end
