defmodule Air.Token do
  @moduledoc "Functions for token management."

  alias Air.{Schemas.ApiToken, Schemas.User, Repo}
  alias AirWeb.Endpoint

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Given a user and a description, a token is created and assigned to the user"
  @spec create_api_token(User.t(), ApiToken.Access.t(), String.t()) :: String.t() | {:error, Ecto.Changeset.t()}
  def create_api_token(user, access, description) do
    changeset =
      ApiToken.changeset(%ApiToken{}, %{
        access: access,
        description: description,
        user_id: user.id
      })

    with {:ok, token_entry} <- Repo.insert(changeset) do
      Phoenix.Token.sign(Endpoint, api_token_salt(), token_entry.id)
    end
  end

  @doc "Will return the user associated with a token, assuming the token is valid"
  @spec user_for_token(String.t(), ApiToken.Access.t(), max_age: pos_integer | :infinity) :: User.t() | :error
  def user_for_token(token, access, opts \\ []) do
    import Ecto.Query

    case Phoenix.Token.verify(Endpoint, api_token_salt(), token, max_age: normalized_max_age(opts[:max_age])) do
      {:ok, token_id} ->
        from(
          token in ApiToken,
          where: token.id == ^token_id,
          where: token.access == ^access,
          inner_join: user in assoc(token, :user),
          where: user.enabled,
          preload: [{:user, :groups}],
          select: token
        )
        |> Repo.one()
        |> case do
          %{} = token ->
            touch_token(token)
            token.user

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  # -------------------------------------------------------------------
  # Salt configuration
  # -------------------------------------------------------------------

  defp normalized_max_age(max_age) when is_integer(max_age), do: max_age

  defp normalized_max_age(nil),
    # default max age is one day
    do: 60 * 60 * 24

  defp normalized_max_age(:infinity),
    # Phoenix warns if we're not validating the token age, so we need to pass some integer value.
    # Therefore, we're simulating infinity by using a ridiculously large value (10,000 years).
    do: 60 * 60 * 24 * 365 * 10_000

  defp api_token_salt do
    Application.get_env(:air, AirWeb.Endpoint) |> Keyword.fetch!(:api_token_salt)
  end

  if Mix.env() == :test do
    # Starting an async task that uses the DB plays badly with the Ecto Sandbox
    defp touch_token(_), do: :ok
  else
    defp touch_token(token), do: Air.ApiTokenTimestampUpdater.start_token_toucher(token)
  end
end
