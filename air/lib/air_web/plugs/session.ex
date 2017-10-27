defmodule AirWeb.Plug.Session do
  @moduledoc false


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  defmodule ApiAuth do
    @moduledoc """
    This plug ensures that callers of our API's supply a valid auth-token header.
    It is not compatible with, and can not be used in conjunction with, the
    plugs for the browser pipelines, as these rely heavily on parameters set
    and validated by Guardian.
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
          |> Phoenix.Controller.json(%{success: false, description: missing_auth_header_error(conn)})
          |> halt()
        token ->
          case Air.Token.user_for_token(token, Keyword.fetch!(opts, :access), max_age: :infinity) do
            :error ->
              conn
              |> put_status(Plug.Conn.Status.code(:unauthorized))
              |> Phoenix.Controller.json(%{success: false, description: invalid_auth_token_error(conn)})
              |> halt()
            user -> assign(conn, :current_user, user)
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
      Plug.Conn.assign(conn, :current_user, Guardian.Plug.current_resource(conn))
    end
  end

  defmodule Restoration do
    @moduledoc """
    Plug that allows us to provide a "remember me" feature in our login system.
    The guardian authentication system checks the session store for the authentication token.
    When a user wants us to remember that he is logged in, we create an additional cookie
    that allows us to restore the session variable on subsequent visits.
    """
    @behaviour Plug

    require Logger

    @cookie_key "auth_remember_me"
    # 30 days in seconds (30*24*60*60) - the time before a user has to login again.
    @cookie_max_age_s 2_592_000


    # -------------------------------------------------------------------
    # Plug callbacks
    # -------------------------------------------------------------------

    @impl Plug
    def init(default), do: default

    @impl Plug
    def call(conn, _default) do
      case Plug.Conn.get_session(conn, session_key()) do
        nil ->
          conditionally_restore_session(conn)
        _ ->
          # A session already exists, so we don't need to do anything at all
          conn
      end
    end


    # -------------------------------------------------------------------
    # Utility functions
    # -------------------------------------------------------------------

    @doc "Persists the user session in the cookie."
    @spec persist_token(Plug.Conn.t) :: Plug.Conn.t
    def persist_token(conn) do
      Logger.debug("The user wants us to remember that s/he is logged in")
      jwt = Plug.Conn.get_session(conn, session_key())
      Plug.Conn.put_resp_cookie(conn, @cookie_key, jwt, max_age: @cookie_max_age_s)
    end

    @doc "Removes the persisted session from the cookie."
    @spec remove_token(Plug.Conn.t) :: Plug.Conn.t
    def remove_token(conn) do
      Logger.debug("The user wants us to forget that s/he was logged in")
      Plug.Conn.delete_resp_cookie(conn, @cookie_key, max_age: @cookie_max_age_s)
    end

    defp conditionally_restore_session(conn) do
      %Plug.Conn{req_cookies: req_cookies} = Plug.Conn.fetch_cookies(conn)
      case req_cookies[@cookie_key] do
        nil ->
          # The user isn't logged in, or didn't use the remember-me feature
          conn
        jwt ->
          Logger.debug("Restoring user session from cookie, logging in the user")
          Plug.Conn.put_session(conn, session_key(), jwt)
      end
    end

    defp session_key() do
      Guardian.Keys.base_key(:default)
    end
  end

  defmodule Authenticated do
    @moduledoc """
    Authenticates the current user and loads the user data.

    The user data will be available in the `conn.assigns.current_user`
    """
    use Plug.Builder

    plug AirWeb.Plug.Session.Restoration
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureAuthenticated, handler: __MODULE__
    plug Guardian.Plug.LoadResource
    plug Guardian.Plug.EnsureResource, handler: __MODULE__
    plug AirWeb.Plug.Session.AssignCurrentUser


    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureResource
    # -------------------------------------------------------------------

    @doc false
    def no_resource(conn, params) do
      conn
      |> Guardian.Plug.sign_out()
      |> unauthenticated(params)
    end


    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def unauthenticated(%Plug.Conn{request_path: path} = conn, _params) do
      conn
      |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
      |> Plug.Conn.put_session(:return_path, path)
      |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.session_path(conn, :new))
    end
  end

  defmodule Anonymous do
    @moduledoc """
    Ensures that the user is anonymous.

    This plug will also assign `nil` to `:current_user` so `conn.assigns.current_user`
    can be safely used in subsequent controllers and views.
    """
    use Plug.Builder

    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureNotAuthenticated, handler: __MODULE__
    plug AirWeb.Plug.Session.AssignCurrentUser


    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureNotAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def already_authenticated(conn, _params) do
      Plug.Conn.send_resp(conn, Plug.Conn.Status.code(:bad_request), "already authenticated")
    end
  end
end
