defmodule Central.Schemas.CustomerExport do
  @moduledoc "Schema for customer exports."

  use Central.Web, :model

  @type t :: %__MODULE__{}

  schema "customer_exports" do
    field :export_id, :integer
    belongs_to :customer, Central.Schemas.Customer
    timestamps()
  end
end

