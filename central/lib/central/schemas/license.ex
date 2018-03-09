defmodule Central.Schemas.License do
  @moduledoc "The customer schema."

  use Central.Web, :model

  alias Central.Schemas.Customer

  @type t :: %__MODULE__{}

  schema "licenses" do
    field :name, :string
    field :length_in_days, :integer
    field :auto_renew, :boolean
    field :revoked, :boolean

    belongs_to :customer, Customer

    timestamps()
  end

  @required_fields ~w(name length_in_days auto_renew revoked)a
  @optional_fields ~w()a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t, Map.t) :: Changeset.t
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 1)
    |> validate_number(:length_in_days, greater_than: 0)
  end
end
