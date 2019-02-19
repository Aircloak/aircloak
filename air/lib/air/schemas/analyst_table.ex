defmodule Air.Schemas.AnalystTable do
  @moduledoc "Schema for analyst tables"
  use Air.Schemas.Base
  require Logger
  require EctoEnum

  @type t :: %__MODULE__{
          name: String.t(),
          sql: String.t(),
          columns: [Air.Schemas.SelectableColumn.t()]
        }

  EctoEnum.defenum(CreationStatus, :creation_status, [:pending, :succeeded, :failed])

  schema "analyst_tables" do
    field(:name, :string)
    field(:sql, :string)
    field(:broken, :boolean, default: false)
    field(:creation_status, CreationStatus, default: :pending)

    embeds_many(:columns, Air.Schemas.SelectableColumn, on_replace: :delete)

    belongs_to(:user, Air.Schemas.User)
    belongs_to(:data_source, Air.Schemas.DataSource)
  end
end
