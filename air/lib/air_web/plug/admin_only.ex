defmodule AirWeb.Plug.AdminOnly do
  @moduledoc """
  This is a simplified version of `AirWeb.VerifyPermissions` that checks that the user has the admin role.
  This simplifies the implementation of the whole admin area of the software, as well as enforces the proper
  authorization for LiveView code (that can't benefit from controller plugs).
  """
  require Logger
  @behaviour Plug

  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    case check_permission(conn) do
      :ok ->
        conn

      {:error, formatted_error} ->
        Logger.info(formatted_error)

        conn
        |> Phoenix.Controller.put_flash(:error, "You are not authorized to view the requested resource.")
        |> Phoenix.Controller.redirect(to: "/")
        |> Plug.Conn.halt()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp check_permission(conn) do
    user = conn.assigns.current_user

    if Air.Schemas.User.admin?(user) do
      :ok
    else
      {:error, "#{conn.request_path} not permitted for #{inspect(user || :anonymous)}"}
    end
  end
end
