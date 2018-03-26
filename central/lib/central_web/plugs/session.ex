defmodule CentralWeb.Plug.Session do
  @moduledoc false

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
    @spec persist_token(Plug.Conn.t()) :: Plug.Conn.t()
    def persist_token(conn) do
      Logger.debug("The user wants us to remember that s/he is logged in")
      jwt = Plug.Conn.get_session(conn, session_key())
      Plug.Conn.put_resp_cookie(conn, @cookie_key, jwt, max_age: @cookie_max_age_s)
    end

    @doc "Removes the persisted session from the cookie."
    @spec remove_token(Plug.Conn.t()) :: Plug.Conn.t()
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

    plug(CentralWeb.Plug.Session.Restoration)
    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureAuthenticated, handler: __MODULE__)
    plug(Guardian.Plug.LoadResource)
    plug(CentralWeb.Plug.Session.AssignCurrentUser)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def unauthenticated(%Plug.Conn{request_path: path} = conn, _params) do
      conn
      |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
      |> Plug.Conn.put_session(:return_path, path)
      |> Phoenix.Controller.redirect(to: CentralWeb.Router.Helpers.session_path(conn, :new))
    end
  end

  defmodule Anonymous do
    @moduledoc """
    Ensures that the user is anonymous.

    This plug will also assign `nil` to `:current_user` so `conn.assigns.current_user`
    can be safely used in subsequent controllers and views.
    """
    use Plug.Builder

    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureNotAuthenticated, handler: __MODULE__)
    plug(CentralWeb.Plug.Session.AssignCurrentUser)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureNotAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def already_authenticated(conn, _params) do
      Plug.Conn.send_resp(conn, Plug.Conn.Status.code(:bad_request), "already authenticated")
    end
  end
end
