defmodule Central.Schemas.Cloak do
  @moduledoc "The air schema."

  use Central.Web, :model

  @type t :: %__MODULE__{
    name: String.t,
    status: Central.Schemas.OnlineStatus.t,
    air: Central.Schemas.Air.t,
    data_sources: :integer
  }

  schema "cloaks" do
    field :name, :string
    field :status, Central.Schemas.OnlineStatus
    field :data_sources, :integer

    belongs_to :air, Central.Schemas.Air

    timestamps()
  end
end
