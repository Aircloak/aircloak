defmodule Central.Schemas.UsageInfo do
  @moduledoc "The usage info schema."

  use Central.Web, :model

  @type t :: %__MODULE__{
    air_utc_time: NaiveDateTime.t,
    data: Map.t,
    customer: Central.Schemas.Customer.t,
    air: Central.Schemas.Air.t,
  }

  schema "usage_info" do
    field :air_utc_time, :naive_datetime
    field :data, :map

    belongs_to :customer, Central.Schemas.Customer
    belongs_to :air, Central.Schemas.Air

    timestamps()
  end
end
