defmodule Air.ApiToken do
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
  use Air.Web, :model

  alias Air.{ApiToken, User, Repo}

  @type t :: %__MODULE__{}

  schema "api_tokens" do
    field :description, :string
    belongs_to :user, User, references: :id

    timestamps
  end

  @required_fields ~w(description user_id)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:token, name: :api_tokens_pkey)
    |> validate_length(:description, min: 3)
  end


  # -------------------------------------------------------------------
  # Token management
  # -------------------------------------------------------------------

  @token_type "api_token"

  @doc "Given a user and a description, a token is created and assigned to the user"
  def create_for(conn, user, description) do
    changeset = ApiToken.changeset(%ApiToken{}, %{description: description,
        user_id: user.id})
    case Air.Repo.insert(changeset) do
      {:ok, token_entry} ->
        Phoenix.Token.sign(conn, @token_type, token_entry.id)
      {:error, _} = error -> error
    end
  end

  @doc "Will return the user associated with a token, assuming the token is valid"
  def user_for_token(conn, token) do
    case Phoenix.Token.verify(conn, @token_type, token) do
      {:ok, token_id} ->
        case Repo.one(from t in ApiToken,
            join: u in User, on: u.id == t.user_id,
            where: t.id == ^token_id,
            select: u) do
          %{} = user -> Repo.preload(user, :organisation)
          _ -> :error
        end
      _ -> :error
    end
  end
end
