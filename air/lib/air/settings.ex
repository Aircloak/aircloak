defmodule Air.Settings do
  @moduledoc "Represents a set of Air-wide options, set by the admin."

  @type t :: %__MODULE__{query_retention_days: integer | :undefined}

  defstruct [:query_retention_days]
end
