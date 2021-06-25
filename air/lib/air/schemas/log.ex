defmodule Air.Schemas.Log do
  @moduledoc "Schema for storing log entries in the DB."

  use Air.Schemas.Base
  require EctoEnum

  EctoEnum.defenum(Source, :log_source, [:air, :cloak, :system])

  schema "logs" do
    field(:timestamp, :naive_datetime_usec)
    field(:hostname, :string)
    field(:source, Source)
    field(:message, :string)
  end
end
