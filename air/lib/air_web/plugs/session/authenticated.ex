defmodule AirWeb.Plug.Session.Authenticated do
  def init(options), do: options

  def call(conn, _opts) do
    conn
    |> Plug.Conn.get_session(AirWeb.Plug.Session.session_key())
    |> Air.Service.RevokableToken.verify(:session, max_age: :infinity)
    |> case do
      {:ok, user_id} ->
        Plug.Conn.assign(conn, :current_user, Air.Service.User.load(user_id))

      _ ->
        conn
        |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
        |> Plug.Conn.put_session(:return_path, conn.request_path)
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.session_path(conn, :new))
        |> Plug.Conn.halt()
    end
  end
end
