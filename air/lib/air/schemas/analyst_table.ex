defmodule Air.Schemas.AnalystTable do
  @moduledoc "Schema for analyst tables"
  use Air.Schemas.Base
  require Logger

  @type t :: %__MODULE__{}

  schema "analyst_tables" do
    field(:name, :string)
    field(:sql, :string)
    field(:registration_info, :string)

    belongs_to(:user, Air.Schemas.User)
    belongs_to(:data_source, Air.Schemas.DataSource)
  end
end
