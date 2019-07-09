defmodule Air.Schemas.Salt do
  @moduledoc "Schema for storing named salt for various cryptographic tasks in the DB."

  use Air.Schemas.Base

  schema "salts" do
    field(:name, :string)
    field(:value, :string)
    timestamps(type: :naive_datetime_usec)
  end
end
