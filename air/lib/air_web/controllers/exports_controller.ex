defmodule AirWeb.ExportsController do
  alias Air.Service.Export

  use Air.Web, :controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{user: :all}

  def show(conn, _params) do
    conn = Plug.Conn.send_chunked(conn, 200)

    conn.assigns.current_user
    |> Export.stream()
    |> Enum.reduce_while(conn, fn chunk, conn ->
      case Plug.Conn.chunk(conn, chunk) do
        {:ok, conn} -> {:cont, conn}
        {:error, :closed} -> {:halt, conn}
      end
    end)
  end
end
