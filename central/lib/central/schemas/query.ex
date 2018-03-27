defmodule Central.Schemas.Query do
  @moduledoc "The query schema."

  use Central.Web, :model

  alias Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "queries" do
    field(:metrics, :map)
    field(:features, :map)
    field(:aux, :map)

    belongs_to(:customer, Central.Schemas.Customer)

    timestamps()
  end

  @required_fields ~w()a
  @optional_fields ~w(metrics features aux)a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t(), Map.t()) :: Changeset.t()
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
