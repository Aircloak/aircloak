defmodule Air.Schemas.SelectableColumn do
  @moduledoc "Schema for columns field used in `Air.Schemas.AnalystTable` and `Air.Schemas.View`."
  use Air.Schemas.Base

  @type t :: %__MODULE__{name: String.t(), type: String.t(), key_type: String.t() | nil, comment: String.t() | nil}
  @derive {Jason.Encoder, only: [:name, :type, :key_type, :access, :comment]}
  @primary_key false

  embedded_schema do
    field(:name, :string)
    field(:type, :string)
    field(:key_type, :string)
    field(:access, :string)
    field(:comment, :string)
  end
end
