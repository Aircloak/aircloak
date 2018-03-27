defmodule Air.Schemas.View do
  @moduledoc "Schema for user-defined views"
  use Air.Schemas.Base
  require Logger

  @type t :: %__MODULE__{}

  schema "views" do
    field(:name, :string)
    field(:sql, :string)
    field(:result_info, :map)
    field(:broken, :boolean)

    belongs_to(:user, Air.Schemas.User)
    belongs_to(:data_source, Air.Schemas.User)
  end
end
