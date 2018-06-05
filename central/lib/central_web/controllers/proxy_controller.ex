defmodule CentralWeb.ProxyController do
  @moduledoc false
  use Central.Web, :controller

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def noop(conn, _params), do: text(conn, "OK")

  def proxy_placeholder(conn, _params), do: text(conn, "When setup, nginx will proxy through to the dashboard")
end
