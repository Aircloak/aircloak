defmodule AirWeb.Plug.Session.EveryoneAllowed do
  @moduledoc """
  A plug that allows authenticated and unauthenticated requests, always setting `conn.assigns.current_user`.

  This plug will never halt the request, whether or not the user is logged in.  However when the user is logged in, the
  user will be assigned to `conn.assigns.current_user`. Otherwise `conn.assigns.current_user` will be set to `nil`.
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
      {:ok, user} -> Plug.Conn.assign(conn, :current_user, user)
      :error -> Plug.Conn.assign(conn, :current_user, nil)
    end
  end
end
