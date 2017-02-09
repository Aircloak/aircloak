defmodule Central.Schemas.Cloak do
  @moduledoc "The air schema."

  use Central.Web, :model

  @type t :: %__MODULE__{
    name: String.t,
    status: Central.Schemas.OnlineStatus.t,
    air: Central.Schemas.Air.t,
    data_source_names: [String.t]
  }

  schema "cloaks" do
    field :name, :string
    field :status, Central.Schemas.OnlineStatus
    field :data_source_names, {:array, :string}

    belongs_to :air, Central.Schemas.Air

    timestamps()
  end
end
