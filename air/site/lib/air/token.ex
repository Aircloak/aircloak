defmodule Air.Token do
  @moduledoc "Functions for token management."

  alias Air.{ApiToken, Endpoint, User, Repo}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Given a user and a description, a token is created and assigned to the user"
  @spec create_api_token(User.t, String.t) :: {:ok, String.t} | {:error, String.t}
  def create_api_token(user, description) do
    changeset = ApiToken.changeset(%ApiToken{}, %{description: description, user_id: user.id})
    case Air.Repo.insert(changeset) do
      {:ok, token_entry} -> Phoenix.Token.sign(Endpoint, api_token_salt(), token_entry.id)
      {:error, _} = error -> error
    end
  end

  @doc "Will return the user associated with a token, assuming the token is valid"
  @spec user_for_token(String.t) :: User.t | :error
  def user_for_token(token) do
    import Ecto.Query

    case Phoenix.Token.verify(Endpoint, api_token_salt(), token) do
      {:ok, token_id} ->
        case Repo.one(from user in User,
            join: token in ApiToken, on: token.user_id == user.id,
            where: token.id == ^token_id,
            join: organisation in assoc(user, :organisation),
            preload: [organisation: organisation],
            select: user) do
          %{} = user -> user
          _ -> :error
        end
      _ -> :error
    end
  end

  @doc "Generates a data source token for the given cloak_id and data source name."
  @spec data_source_token(String.t | nil, String.t | nil) :: nil
  def data_source_token(nil, nil), do: nil
  def data_source_token(cloak_id, data_source) do
    Phoenix.Token.sign(Endpoint, data_source_token_salt(), {cloak_id, data_source})
  end

  @doc "Unpacks a data source token."
  @spec decode_data_source_token(String.t | nil) ::  {String.t | nil, String.t | nil}
  def decode_data_source_token(nil), do: {nil, nil}
  def decode_data_source_token(data_source_token) do
    {:ok, {cloak_id, data_source}} = Phoenix.Token.verify(Endpoint, data_source_token_salt(), data_source_token)
    {cloak_id, data_source}
  end


  # -------------------------------------------------------------------
  # Salt configuration
  # -------------------------------------------------------------------

  defp api_token_salt do
    Application.get_env(:air, Air.Endpoint) |> Keyword.fetch!(:api_token_secret)
  end

  defp data_source_token_salt do
    Application.get_env(:air, Endpoint) |> Keyword.fetch!(:data_source_token_secret)
  end
end
