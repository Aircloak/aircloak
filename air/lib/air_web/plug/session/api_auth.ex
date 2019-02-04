defmodule AirWeb.Plug.Session.ApiAuth do
  @moduledoc """
  This plug ensures that callers of our APIs supply a valid auth-token header. It conflicts with
  `AirWeb.Plug.Session.Authenticated` and `AirWeb.Plug.Session.EveryoneAllowed`, as all of them set the `current_user`
  assign.
  """
  @behaviour Plug

  import Plug.Conn

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    conn = Plug.Conn.fetch_query_params(conn)

    case get_token(conn) do
      :error ->
        conn
        |> put_status(Plug.Conn.Status.code(:unauthorized))
        |> Phoenix.Controller.json(%{
          success: false,
          description: missing_auth_header_error(conn)
        })
        |> halt()

      token ->
        case Air.Service.Token.user_for_token(token, Keyword.fetch!(opts, :access), max_age: :infinity) do
          :error ->
            conn
            |> put_status(Plug.Conn.Status.code(:unauthorized))
            |> Phoenix.Controller.json(%{
              success: false,
              description: invalid_auth_token_error(conn)
            })
            |> halt()

          user ->
            assign(conn, :current_user, user)
        end
    end
  end

  # -------------------------------------------------------------------
  # API error messages
  # -------------------------------------------------------------------

  defp site_url(conn), do: "#{conn.scheme}://#{conn.host}:#{conn.port}"

  defp missing_auth_header_error(conn) do
    "The Aircloak API's are authenticated with auth-tokens. You can create auth-tokens for your account " <>
      "at #{site_url(conn)}/api_tokens. The token should be sent with your request via the HTTP " <>
      "header 'auth-token'. For example, using curl, you would make your request like this: " <>
      "`curl -H 'auth-token:<token-value>' ...` where <token-value> is your auth token."
  end

  defp invalid_auth_token_error(conn) do
    "Invalid auth-token. This could be a result of the auth-token being incorrectly sent to the API backend, " <>
      "or the auth-token having been revoked. You can validate that your auth-token is still valid by visiting " <>
      "#{site_url(conn)}/api_tokens."
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def get_token(conn) do
    case Plug.Conn.get_req_header(conn, "auth-token") do
      [token] -> token
      _ -> Map.get(conn.params, "auth_token", :error)
    end
  end
end
