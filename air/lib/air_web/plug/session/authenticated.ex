defmodule AirWeb.Plug.Session.Authenticated do
  @moduledoc """
  A plug that ensures the request is authenticated with a session.

  It will block the request if no valid session token is found. If one is found it will set `conn.assigns.current_user`
  to the user indicated in that token.
  """

  @behaviour Plug

  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(options), do: options

  @impl Plug
  def call(conn, _opts) do
    case AirWeb.Plug.Session.load_user(conn) do
      {:ok, user} ->
        Plug.Conn.assign(conn, :current_user, user)

      _ ->
        conn
        |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page.")
        |> Plug.Conn.put_session(:return_path, conn.request_path)
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.session_path(conn, :new))
        |> Plug.Conn.halt()
    end
  end
end
