defmodule Air.Schemas.View do
  @moduledoc "Schema for user-defined views"
  use Air.Schemas.Base
  require Logger
  require EctoEnum

  @type t :: %__MODULE__{}

  EctoEnum.defenum(CreationStatus, :creation_status, [:succeeded])

  schema "views" do
    field(:name, :string)
    field(:sql, :string)
    field(:result_info, :map)
    field(:broken, :boolean)
    field(:creation_status, CreationStatus, default: :succeeded)

    belongs_to(:user, Air.Schemas.User)
    belongs_to(:data_source, Air.Schemas.User)
  end
end
