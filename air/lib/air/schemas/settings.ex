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

    timestamps(type: :naive_datetime_usec)
  end
end
