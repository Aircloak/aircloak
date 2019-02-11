defmodule Air.Schemas.AnalystTable do
  @moduledoc "Schema for analyst tables"
  use Air.Schemas.Base
  require Logger
  require EctoEnum

  @type t :: %__MODULE__{
          name: String.t(),
          sql: String.t(),
          result_info: __MODULE__.ResultInfo.t() | nil
        }

  EctoEnum.defenum(CreationStatus, :creation_status, [:pending, :succeeded, :failed])

  schema "analyst_tables" do
    field(:name, :string)
    field(:sql, :string)
    field(:broken, :boolean, default: false)
    field(:creation_status, CreationStatus, default: :pending)

    embeds_one :result_info, ResultInfo,
      on_replace: :update,
      primary_key: {:registration_info, :string, autogenerate: false} do
      @type t :: %__MODULE__{registration_info: String.t(), columns: [__MODULE__.Column.t()]}

      embeds_many :columns, Column, on_replace: :delete, primary_key: false do
        @type t :: %__MODULE__{name: String.t(), type: String.t(), user_id: boolean}
        @derive {Jason.Encoder, only: [:name, :type, :user_id]}

        field(:name, :string)
        field(:type, :string)
        field(:user_id, :boolean)
      end
    end

    belongs_to(:user, Air.Schemas.User)
    belongs_to(:data_source, Air.Schemas.DataSource)
  end
end
