defmodule AirWeb.ExportsController do
  alias Air.Service.Export

  use Air.Web, :controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{user: :all}

  def show(conn, _params) do
    conn =
      conn
      |> put_resp_content_type("application/json")
      |> Plug.Conn.send_chunked(200)

    {:ok, conn} =
      Export.reduce_while(conn.assigns.current_user, conn, fn chunk, conn ->
        case Plug.Conn.chunk(conn, chunk) do
          {:ok, conn} -> {:cont, conn}
          {:error, :closed} -> {:halt, conn}
        end
      end)

    conn
  end
end
