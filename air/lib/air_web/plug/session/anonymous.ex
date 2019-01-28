defmodule AirWeb.Plug.Session.Anonymous do
  def init(options), do: options

  def call(conn, _opts) do
    case AirWeb.Plug.Session.load_user(conn) do
      :error -> Plug.Conn.assign(conn, :current_user, nil)
      {:ok, _user} -> conn |> Phoenix.Controller.redirect(to: "/") |> Plug.Conn.halt()
    end
  end
end
