defmodule Air.Schemas.License do
  @moduledoc "Schema for serializing licenses into the DB."

  use Air.Schemas.Base

  @type t :: %__MODULE__{}

  schema "licenses" do
    field(:text, :string)
    timestamps()
  end

  @required_fields ~w(text)a
  @optional_fields ~w()a

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
