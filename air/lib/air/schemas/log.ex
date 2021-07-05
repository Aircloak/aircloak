defmodule Air.Schemas.Log do
  @moduledoc "Schema for storing log entries in the DB."

  use Air.Schemas.Base
  require EctoEnum

  EctoEnum.defenum(Source, :log_source, [:air, :cloak, :system])

  EctoEnum.defenum(Level, :log_level, [:debug, :info, :warn, :error])

  @type t :: %__MODULE__{
          timestamp: NaiveDateTime.t(),
          source: Source,
          hostname: String.t(),
          level: Level,
          message: String.t()
        }

  schema "logs" do
    field(:timestamp, :naive_datetime_usec)
    field(:source, Source)
    field(:hostname, :string)
    field(:level, Level)
    field(:message, :string)
  end
end
