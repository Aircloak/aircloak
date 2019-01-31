defmodule AirWeb.Plug.Session.Anonymous do
  @moduledoc """
  A plug that ensures the request is not authenticated with a session.

  If a valid session token is found, it will redirect to `/` and halt the request. Otherwise, it will set
  `conn.assigns.current_user` to nil and let the request through.
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
      :error -> Plug.Conn.assign(conn, :current_user, nil)
      {:ok, _user} -> conn |> Phoenix.Controller.redirect(to: "/") |> Plug.Conn.halt()
    end
  end
end
