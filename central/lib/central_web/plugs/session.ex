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
      Plug.Conn.assign(conn, :current_user, Central.Guardian.Plug.current_resource(conn))
    end
  end

  defmodule Restoration do
    @moduledoc """
    Plug that allows us to provide a "remember me" feature in our login system.  The guardian authentication system
    checks the session store for the authentication token.  When a user wants us to remember that he is logged in, we
    create an additional cookie that allows us to restore the session variable on subsequent visits. This implementation
    is currently adapted from https://github.com/ueberauth/guardian/blob/master/lib/guardian/plug/verify_cookie.ex.
    Once guardian 1.1.0 is released we should just use the implementation from the lib.
    """
    @behaviour Plug

    require Logger
    alias Central.Schemas.User

    @cookie_key "central_auth_remember_me"
    # 30 days in seconds (30*24*60*60) - the time before a user has to login again.
    @cookie_max_age_s 2_592_000

    # -------------------------------------------------------------------
    # Plug callbacks
    # -------------------------------------------------------------------

    @impl Plug
    def init(default), do: default

    @impl Plug
    def call(%{req_cookies: %Plug.Conn.Unfetched{}} = conn, opts) do
      conn
      |> Plug.Conn.fetch_cookies()
      |> call(opts)
    end

    def call(conn, opts) do
      with nil <- Guardian.Plug.current_token(conn, opts),
           {:ok, token} <- find_token_from_cookies(conn),
           active_session? <- Guardian.Plug.session_active?(conn),
           exchange_to <- Central.Guardian.default_token_type(),
           {:ok, _old, {new_token, new_claims}} <- Central.Guardian.exchange(token, "refresh", exchange_to) do
        conn
        |> Central.Guardian.Plug.put_current_token(new_token)
        |> Central.Guardian.Plug.put_current_claims(new_claims)
        |> maybe_put_in_session(active_session?, new_token, opts)
      else
        _ -> conn
      end
    end

    # -------------------------------------------------------------------
    # Utility functions
    # -------------------------------------------------------------------

    @doc "Persists the user session in the cookie."
    @spec persist_token(Plug.Conn.t(), User.t()) :: Plug.Conn.t()
    def persist_token(conn, user) do
      Logger.debug("The user wants us to remember that s/he is logged in")

      case Central.Guardian.encode_and_sign(user, _claims = %{}, token_type: "refresh") do
        {:ok, token, _new_claims} -> Plug.Conn.put_resp_cookie(conn, @cookie_key, token, max_age: @cookie_max_age_s)
        {:error, error} -> raise error
      end
    end

    @doc "Removes the persisted session from the cookie."
    @spec remove_token(Plug.Conn.t()) :: Plug.Conn.t()
    def remove_token(conn) do
      Logger.debug("The user wants us to forget that s/he was logged in")
      Plug.Conn.delete_resp_cookie(conn, @cookie_key, max_age: @cookie_max_age_s)
    end

    defp find_token_from_cookies(conn) do
      token = conn.req_cookies[@cookie_key]
      if token, do: {:ok, token}, else: :no_token_found
    end

    defp maybe_put_in_session(conn, false, _, _), do: conn

    defp maybe_put_in_session(conn, true, token, opts) do
      key = conn |> storage_key(opts) |> Guardian.Plug.Keys.token_key()
      Plug.Conn.put_session(conn, key, token)
    end

    defp storage_key(conn, opts), do: Guardian.Plug.Pipeline.fetch_key(conn, opts)
  end

  defmodule Authenticated do
    @moduledoc """
    Authenticates the current user and loads the user data.

    The user data will be available in the `conn.assigns.current_user`
    """
    use Guardian.Plug.Pipeline, otp_app: :central, module: Central.Guardian, error_handler: __MODULE__

    plug(CentralWeb.Plug.Session.Restoration)
    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureAuthenticated)
    plug(Guardian.Plug.LoadResource)
    plug(CentralWeb.Plug.Session.AssignCurrentUser)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.Pipeline
    # -------------------------------------------------------------------

    @doc false
    def auth_error(%Plug.Conn{request_path: path} = conn, {:unauthenticated, _}, _params) do
      conn
      |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
      |> Plug.Conn.put_session(:return_path, path)
      |> Phoenix.Controller.redirect(to: CentralWeb.Router.Helpers.session_path(conn, :new))
    end
  end

  defmodule EveryoneAllowed do
    @moduledoc """
    This plug will never halt the request, whether or not the user is logged in.
    However when the user is logged in, the user will be assigned to `:current_user`.
    """
    use Guardian.Plug.Pipeline, otp_app: :central, module: Central.Guardian, error_handler: __MODULE__

    plug(Guardian.Plug.VerifySession)
    plug(CentralWeb.Plug.Session.AssignCurrentUser)

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
    use Guardian.Plug.Pipeline, otp_app: :central, module: Central.Guardian, error_handler: __MODULE__

    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureNotAuthenticated)
    plug(CentralWeb.Plug.Session.AssignCurrentUser)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.Pipeline
    # -------------------------------------------------------------------

    @doc false
    def auth_error(conn, {:already_authenticated, _}, _params) do
      Plug.Conn.send_resp(conn, Plug.Conn.Status.code(:bad_request), "already authenticated")
    end
  end

  defmodule HaltIfNotAuthenticated do
    @moduledoc """
    This plug can be used to check if a user is authenticated or not.
    If the check fails, the plug pipeline is halted and a 401 unauthorized error produced.
    If the check passes, the connection is left unaltered and let pass.
    Unlike the other plugs, it does not load the user on succesful authentication.

    Useful as an internal authentication callback for nginx for requests
    that should otherwise be proxied and handled by nginx itself.

    Common usage would look like:

      pipe_through([..., CentralWeb.Plug.Session.HaltIfNotAuthenticated])
      get("/", SomeNoopController, :get)
    """

    use Guardian.Plug.Pipeline,
      otp_app: :central,
      module: Central.Guardian,
      error_handler: __MODULE__

    plug(CentralWeb.Plug.Session.Restoration)
    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureAuthenticated)

    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.Pipeline
    # -------------------------------------------------------------------

    @doc false
    def auth_error(conn, {:unauthenticated, _}, _params) do
      conn
      |> Plug.Conn.put_status(:unauthorized)
      |> Phoenix.Controller.text("Unauthorized: please log in to Central first")
      |> Plug.Conn.halt()
    end
  end
end
