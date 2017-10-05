defmodule Air.Token do
  @moduledoc "Functions for token management."

  alias Air.{Schemas.ApiToken, Endpoint, Schemas.User, Repo}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Given a user and a description, a token is created and assigned to the user"
  @spec create_api_token(User.t, ApiToken.Access.t, String.t) :: String.t | {:error, Ecto.Changeset.t}
  def create_api_token(user, access, description) do
    changeset = ApiToken.changeset(%ApiToken{}, %{access: access, description: description, user_id: user.id})
    with {:ok, token_entry} <- Repo.insert(changeset) do
      Phoenix.Token.sign(Endpoint, api_token_salt(), token_entry.id)
    end
  end

  @doc "Will return the user associated with a token, assuming the token is valid"
  @spec user_for_token(String.t, ApiToken.Access.t) :: User.t | :error
  def user_for_token(token, access) do
    import Ecto.Query

    case Phoenix.Token.verify(Endpoint, api_token_salt(), token) do
      {:ok, token_id} ->
        case Repo.one(from token in ApiToken,
            where: token.id == ^token_id,
            where: token.access == ^access,
            preload: [{:user, :groups}],
            select: token) do
          %{} = token ->
            touch_token(token)
            token.user
          _ -> :error
        end
      _ -> :error
    end
  end


  # -------------------------------------------------------------------
  # Salt configuration
  # -------------------------------------------------------------------

  defp api_token_salt do
    Application.get_env(:air, Air.Endpoint) |> Keyword.fetch!(:api_token_salt)
  end

  if Mix.env == :test do
    # Starting an async task that uses the DB plays badly with the Ecto Sandbox
    defp touch_token(_), do: :ok
  else
    defp touch_token(token), do:
      Air.ApiTokenTimestampUpdater.start_token_toucher(token)
  end
end
