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
          |> Phoenix.Controller.json(%{
            success: false,
            description: missing_auth_header_error(conn)
          })
          |> halt()

        token ->
          case Air.Token.user_for_token(token, Keyword.fetch!(opts, :access), max_age: :infinity) do
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
    alias Air.Schemas.User

    @cookie_key "auth_remember_me"
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
           exchange_to <- Air.Guardian.default_token_type(),
           {:ok, _old, {new_token, new_claims}} <- Air.Guardian.exchange(token, "refresh", exchange_to) do
        conn
        |> Air.Guardian.Plug.put_current_token(new_token)
        |> Air.Guardian.Plug.put_current_claims(new_claims)
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

      case Air.Guardian.encode_and_sign(user, _claims = %{}, token_type: "refresh") do
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
    use Guardian.Plug.Pipeline, otp_app: :air, module: Air.Guardian, error_handler: __MODULE__

    plug(AirWeb.Plug.Session.Restoration)
    plug(Guardian.Plug.VerifySession)
    plug(Guardian.Plug.EnsureAuthenticated)
    plug(Guardian.Plug.LoadResource)
    plug(AirWeb.Plug.Session.AssignCurrentUser)

    @doc false
    def auth_error(conn, {:no_resource_found, reason}, params) do
      conn
      |> Air.Guardian.Plug.sign_out()
      |> auth_error({:unauthenticated, reason}, params)
    end

    def auth_error(%Plug.Conn{request_path: path} = conn, {:unauthenticated, _}, _params) do
      conn
      |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
      |> Plug.Conn.put_session(:return_path, path)
      |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.session_path(conn, :new))
    end
  end

  defmodule EveryoneAllowed do
    @moduledoc """
    This plug will never halt the request, whether or not the user is logged in.
    However when the user is logged in, the user will be assigned to `:current_user`.
    """
    use Guardian.Plug.Pipeline, otp_app: :central, module: Air.Guardian, error_handler: __MODULE__

    plug(Guardian.Plug.VerifySession)
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
  end
end
