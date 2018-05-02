defmodule Air.Schemas.AuditLog do
  @moduledoc "Model for recording events having taken place"
  use Air.Schemas.Base
  alias Air.Schemas.User
  require Logger

  @type t :: %__MODULE__{}

  schema "audit_logs" do
    field(:event, :string)
    field(:metadata, :map)
    belongs_to(:user, User)

    timestamps()
  end

  @required_fields ~w(event metadata)a
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
