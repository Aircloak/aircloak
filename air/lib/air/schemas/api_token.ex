defmodule Air.Schemas.ApiToken do
  @moduledoc """
  The API tokens allow us to authenticate users accessing our API.
  There is a many tokens to a single user relationship, allowing a
  user to have different API tokens for different services, and to
  individually revoke them.

  The token itself is not recorded in the database, and only shown
  to the client generating it. The token encodes the ID of the token
  record in the database, allowing us to validate the existence of
  the token when the request is made.
  """
  use Air.Schemas.Base

  alias Ecto.Changeset

  require EctoEnum

  EctoEnum.defenum(Access, :api_token_type, [:api, :monitoring])

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "api_tokens" do
    field(:description, :string)
    field(:access, __MODULE__.Access)
    belongs_to(:user, Air.Schemas.User, references: :id)

    timestamps()
  end

  @required_fields ~w(description user_id access)a
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
    |> unique_constraint(:token, name: :api_tokens_pkey)
    |> validate_length(:description, min: 3)
  end

  @doc """
  Updates the token with information about when it was last used,
  and other stats, like how frequently it has been accessed etc.

  This allows a user to determine whether or not a token is still
  in active use, or whether it can be removed.
  """
  @spec touch(ApiToken.t()) :: Changeset.t()
  def touch(model) do
    time_params = %{updated_at: NaiveDateTime.utc_now()}
    cast(model, time_params, [:updated_at])
  end
end
