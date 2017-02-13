defmodule Central.Schemas.Air do
  @moduledoc "The air schema."

  use Central.Web, :model

  @type t :: %__MODULE__{
    name: String.t,
    version: String.t,
    status: Central.Schemas.OnlineStatus.t,
    customer: Central.Schemas.Customer.t,
    cloaks: [Central.Schemas.Cloak.t],
  }

  schema "airs" do
    field :name, :string
    field :version, :string
    field :status, Central.Schemas.OnlineStatus

    belongs_to :customer, Central.Schemas.Customer
    has_many :cloaks, Central.Schemas.Cloak

    timestamps()
  end
end
