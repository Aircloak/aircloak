defmodule Air.Service.Token do
  @moduledoc "Functions for token management."

  alias Air.Repo
  alias Air.Schemas.{ApiToken, User, Query}
  alias Air.Service.Salts
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

  def find_token_for_user(user, description) do
    import Ecto.Query

    token =
      from(token in Air.Schemas.ApiToken, where: token.user_id == ^user.id and token.description == ^description)
      |> Repo.one()

    Phoenix.Token.sign(Endpoint, api_token_salt(), token.id)
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
          preload: [{:user, [:groups, :logins]}],
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

  @doc "Returns a token representing the right to view the given query without authenticating."
  @spec public_query_token(Query.t()) :: String.t()
  def public_query_token(query) do
    Phoenix.Token.sign(Endpoint, Salts.get(:query_permalink), {:public, :query, query.id})
  end

  @doc "Returns a token representing the right to view the given query while having access to its data source."
  @spec private_query_token(Query.t()) :: String.t()
  def private_query_token(query) do
    Phoenix.Token.sign(Endpoint, Salts.get(:query_permalink), {:private, :query, query.id})
  end

  @doc """
  Returns the query specified by the given token. If the query is private, it's only returned if the user can access its
  data source. Returns `:error` if the token is invalid or the query inaccessible.
  """
  @spec query_from_token(User.t(), String.t()) :: {:ok, Query.t()} | :error
  def query_from_token(user, token) do
    with {:ok, token} <- Phoenix.Token.verify(Endpoint, Salts.get(:query_permalink), token, max_age: :infinity),
         {:ok, query} <- do_query_from_token(user, token) do
      {:ok, query}
    else
      _ -> :error
    end
  end

  @doc "Returns the type of the given query token."
  @spec query_token_type!(String.t()) :: :public | :private
  def query_token_type!(token) do
    case Phoenix.Token.verify(Endpoint, Salts.get(:query_permalink), token, max_age: :infinity) do
      {:ok, {type, :query, _id}} when type in [:public, :private] -> type
      _ -> raise "invalid query token"
    end
  end

  # -------------------------------------------------------------------
  # Query from token
  # -------------------------------------------------------------------

  defp do_query_from_token(_user, {:public, :query, id}), do: Air.Service.Query.get(id)

  defp do_query_from_token(nil, {:private, :query, _id}), do: :error

  defp do_query_from_token(user, {:private, :query, id}) do
    with {:ok, query} <- Air.Service.Query.get(id),
         {:ok, _data_source} <- Air.Service.DataSource.fetch_as_user({:id, query.data_source_id}, user) do
      {:ok, query}
    end
  end

  # -------------------------------------------------------------------
  # Salt configuration
  # -------------------------------------------------------------------

  defp normalized_max_age(max_age) when is_integer(max_age), do: max_age
  defp normalized_max_age(nil), do: _one_day = 60 * 60 * 24
  defp normalized_max_age(:infinity), do: :infinity

  defp api_token_salt(), do: legacy_salt() || Salts.get(:api_token)

  defp legacy_salt() do
    require Aircloak.DeployConfig

    case Aircloak.DeployConfig.fetch("site") do
      :error -> nil
      {:ok, settings} -> settings["api_token_salt"]
    end
  end

  defp touch_token(token), do: Air.TimestampUpdater.start_toucher(token)
end
