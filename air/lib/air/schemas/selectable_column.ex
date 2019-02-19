defmodule Air.Schemas.SelectableColumn do
  @moduledoc "Schema for columns field used in `Air.Schemas.AnalystTable` and `Air.Schemas.View`."
  use Air.Schemas.Base

  @type t :: %__MODULE__{name: String.t(), type: String.t(), user_id: boolean}
  @derive {Jason.Encoder, only: [:name, :type, :user_id]}

  embedded_schema do
    field(:name, :string)
    field(:type, :string)
    field(:user_id, :boolean)
  end
end
