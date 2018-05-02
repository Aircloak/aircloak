defmodule AirWeb.ExportsController do
  alias Air.Service.Export

  use Air.Web, :controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{user: :all}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, _params) do
    conn = put_resp_content_type(conn, "application/json")

    Export.reduce_while(conn.assigns.current_user, conn, fn chunk, conn ->
      conn
      |> send_chunked()
      |> Plug.Conn.chunk(chunk)
      |> case do
        {:ok, conn} -> {:cont, conn}
        {:error, :closed} -> {:halt, conn}
      end
    end)
    |> case do
      {:ok, conn} ->
        conn

      {:error, :export_in_progress} ->
        Plug.Conn.send_resp(conn, 403, ~s("Another export in progress"))
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_chunked(conn = %Plug.Conn{state: :chunked}), do: conn
  defp send_chunked(conn), do: Plug.Conn.send_chunked(conn, 200)
end
