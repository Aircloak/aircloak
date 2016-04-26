defmodule Air.Plug.Session do
  @moduledoc false

  defmodule AssignCurrentUser do
    @moduledoc false
    @behaviour Plug

    def init(opts), do: opts

    def call(conn, _opts) do
      Plug.Conn.assign(conn, :current_user, Guardian.Plug.current_resource(conn))
    end
  end

  defmodule Authenticated do
    @moduledoc """
    Authenticates the current user and loads the user data.

    The user data will be available in the `conn.assigns.current_user`
    """
    use Plug.Builder

    plug Air.Plugs.GuardianSessionRestoration
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.EnsureAuthenticated, handler: __MODULE__
    plug Guardian.Plug.LoadResource
    plug Air.Plug.Session.AssignCurrentUser


    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def unauthenticated(%Plug.Conn{request_path: path} = conn, _params) do
      conn
      |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
      |> Plug.Conn.put_session(:return_path, path)
      |> Phoenix.Controller.redirect(to: Air.Router.Helpers.session_path(conn, :new))
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
    plug Air.Plug.Session.AssignCurrentUser


    # -------------------------------------------------------------------
    # Callback for Guardian.Plug.EnsureNotAuthenticated
    # -------------------------------------------------------------------

    @doc false
    def already_authenticated(conn, _params) do
      Plug.Conn.send_resp(conn, Plug.Conn.Status.code(:bad_request), "already authenticated")
    end
  end
end
