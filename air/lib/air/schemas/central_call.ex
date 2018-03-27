defmodule Air.Schemas.CentralCall do
  @moduledoc "This model holds RPC calls from the air to the central that are exported manually."
  use Air.Schemas.Base

  alias Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "central_calls" do
    field(:event, :string)
    field(:payload, :map)

    timestamps()
  end

  @required_fields ~w(event payload)a
  @optional_fields ~w()a

  # -------------------------------------------------------------------
  # API
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
