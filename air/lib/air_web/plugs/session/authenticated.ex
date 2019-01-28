defmodule AirWeb.Plug.Session.Authenticated do
  def init(options), do: options

  def call(conn, _opts) do
    with {:ok, user_id} <- unpack_session(conn),
         {:ok, user} <- Air.Service.User.load_enabled(user_id) do
      Plug.Conn.assign(conn, :current_user, user)
    else
      _ ->
        conn
        |> Phoenix.Controller.put_flash(:error, "You must be authenticated to view this page")
        |> Plug.Conn.put_session(:return_path, conn.request_path)
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.session_path(conn, :new))
        |> Plug.Conn.halt()
    end
  end

  defp unpack_session(conn) do
    conn
    |> Plug.Conn.get_session(AirWeb.Plug.Session.session_key())
    |> Air.Service.RevokableToken.verify(:session, max_age: :infinity)
  end
end
