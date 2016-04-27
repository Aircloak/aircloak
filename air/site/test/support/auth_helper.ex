defmodule Air.TestAuthHelper do
  @moduledoc "Helpers for working with users."

  alias Air.{ApiToken, User}

  defmodule TokenEndpoint do
    @moduledoc false
    def config(:secret_key_base), do: "abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234"
  end

  @doc "Creates a token that can be used in API calls"
  @spec create_token(Plug.Conn.t, User.t) :: String.t
  def create_token(conn, user) do
    ApiToken.create_for(conn, user, "Test token")
  end

  @doc "Creates a token instance and returns it. No token string is generated"
  @spec create_token(Plug.Conn.t, User.t) :: ApiToken.t
  def create_token_entity!(user) do
    changeset = ApiToken.changeset(%ApiToken{}, %{description: "test token", user_id: user.id})
    Air.Repo.insert!(changeset)
  end

  @doc """
  Adds a test endpoint to the connection, containing a secret key. This is required in order to be
  able to create tokens for a user, before the connection has been used to issue a request (at which
  point the endpoint is replaced
  """
  @spec add_auth_to_conn(Plug.Conn.t) :: Plug.Conn.t
  def add_auth_to_conn(conn) do
    Plug.Conn.put_private(conn, :phoenix_endpoint, TokenEndpoint)
  end
end
