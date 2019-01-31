defmodule Air.Schemas.RevokableToken do
  @moduledoc """
  The revokable tokens allow us to create tokens that can be revoked by a server-side decision. This is useful to:

  1. Create one-time tokens, for example for password reset
  2. Create revokable sessions
  """
  use Air.Schemas.Base

  alias Ecto.Changeset

  require EctoEnum

  EctoEnum.defenum(RevokableTokenType, :revokable_token_type, [:session, :password_reset])

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "revokable_tokens" do
    belongs_to(:user, Air.Schemas.User, references: :id)

    field(:type, __MODULE__.RevokableTokenType)
    field(:payload, :binary)
    field(:valid_until, :naive_datetime)

    timestamps()
  end

  @required_fields ~w(type payload valid_until)a
  @optional_fields ~w()a

  @doc """
  Creates a changeset based on the `model` and `params`. If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t(), Map.t()) :: Changeset.t()
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
