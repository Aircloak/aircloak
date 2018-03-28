defmodule Air.TestAuthHelper do
  @moduledoc "Helpers for working with users."

  require Aircloak.DeployConfig
  alias Air.{Schemas.ApiToken, Schemas.User, Token}

  defmodule TokenEndpoint do
    @moduledoc false
    def config(:secret_key_base), do: Map.fetch!(Aircloak.DeployConfig.fetch!("site"), "endpoint_key_base")
  end

  @doc "Creates a token that can be used in API calls"
  @spec create_token(User.t()) :: String.t()
  def create_token(user) do
    Token.create_api_token(user, :api, "Test token")
  end

  @doc "Creates a token instance and returns it. No token string is generated"
  @spec create_token_entity!(User.t()) :: ApiToken.t()
  def create_token_entity!(user) do
    changeset = ApiToken.changeset(%ApiToken{}, %{description: "test token", access: :api, user_id: user.id})

    Air.Repo.insert!(changeset)
  end

  @doc """
  Adds a test endpoint to the connection, containing a secret key. This is required in order to be
  able to create tokens for a user, before the connection has been used to issue a request (at which
  point the endpoint is replaced
  """
  @spec add_auth_to_conn(Plug.Conn.t()) :: Plug.Conn.t()
  def add_auth_to_conn(conn) do
    Plug.Conn.put_private(conn, :phoenix_endpoint, TokenEndpoint)
  end
end
