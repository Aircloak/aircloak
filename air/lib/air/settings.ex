defmodule Air.Settings do
  @moduledoc "Represents a set of Air-wide options, set by the admin."

  @type t :: %__MODULE__{
    query_retention_days: integer | :undefined,
    audit_log_enabled: boolean
  }

  defstruct [:query_retention_days, :audit_log_enabled]
end
