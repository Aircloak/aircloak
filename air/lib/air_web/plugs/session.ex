defmodule AirWeb.Plug.Session do
  @moduledoc "Base module for plugs dealing with authentication."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  alias Air.Service.RevokableToken

  @doc "Returns the name of the session key where the session id is stored."
  @spec session_key() :: String.t()
  def session_key(), do: "_air_session_token"

  @doc "Creates a new session token for the user and sets it in the given conn."
  @spec sign_in(Plug.Conn.t(), Air.Schemas.User.t()) :: Plug.Conn.t()
  def sign_in(conn, user), do: Plug.Conn.put_session(conn, session_key(), RevokableToken.sign(user.id, user, :session))

  @doc "Revokes the session token in the given conn."
  @spec sign_out(Plug.Conn.t()) :: Plug.Conn.t()
  def sign_out(conn) do
    conn |> Plug.Conn.get_session(session_key()) |> RevokableToken.revoke(:session)
    Plug.Conn.delete_session(conn, session_key())
  end

  @doc "Returns the current session token."
  @spec current_token(Plug.Conn.t()) :: String.t()
  def current_token(conn), do: Plug.Conn.get_session(conn, session_key())

  @doc "Returns true if the connection is authenticated (as any user), false otherwise."
  @spec authenticated?(Plug.Conn.t()) :: boolean()
  def authenticated?(conn), do: not is_nil(conn.assigns.current_user)

  defmodule ApiAuth do
    @moduledoc """
    This plug ensures that callers of our APIs supply a valid auth-token header. It is not compatible with, and can not
    be used in conjunction with, the plugs for the browser pipelines, as these rely heavily on parameters set and
    validated by Guardian.
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

  # -------------------------------------------------------------------
  # Browser
  # -------------------------------------------------------------------

  defmodule AssignCurrentUser do
    @moduledoc false
    @behaviour Plug

    def init(opts), do: opts

    def call(conn, _opts) do
      Plug.Conn.assign(conn, :current_user, Air.Guardian.Plug.current_resource(conn))
    end
  end

  defmodule EveryoneAllowed do
    @moduledoc """
    This plug will never halt the request, whether or not the user is logged in.
    However when the user is logged in, the user will be assigned to `:current_user`.
    """
    use Guardian.Plug.Pipeline, otp_app: :central, module: Air.Guardian, error_handler: __MODULE__

    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.LoadResource, allow_blank: true)
    plug(AirWeb.Plug.Session.AssignCurrentUser)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.Pipeline
    # -------------------------------------------------------------------

    @doc false
    def auth_error(conn, _error, _params), do: conn
  end

  defmodule Anonymous do
    @moduledoc """
    Ensures that the user is anonymous.

    This plug will also assign `nil` to `:current_user` so `conn.assigns.current_user`
    can be safely used in subsequent controllers and views.
    """
    use Guardian.Plug.Pipeline, otp_app: :air, module: Air.Guardian, error_handler: __MODULE__

    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureNotAuthenticated)
    plug(AirWeb.Plug.Session.AssignCurrentUser)

    @doc false
    def auth_error(conn, {:already_authenticated, _}, _params) do
      Phoenix.Controller.redirect(conn, to: "/")
    end

    def auth_error(conn, {:invalid_token, _}, _params) do
      conn
      |> Air.Guardian.Plug.sign_out()
      |> Phoenix.Controller.redirect(to: "/auth")
    end
  end
end
