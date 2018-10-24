defmodule AirWeb.Admin.LicenseController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.License

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all,
      user: [:invalid]
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def edit(conn, _params) do
    render(conn, "edit.html")
  end

  def update(conn, %{"license" => %{"text" => %Plug.Upload{path: path}}}) do
    with {:ok, text} <- File.read(path) do
      case License.load(text) do
        :ok -> redirect_to_edit(conn, :info, "License uploaded")
        {:error, reason} -> redirect_to_edit(conn, :error, reason)
      end
    else
      _ -> redirect_to_edit(conn, :error, "Unknown error. Please try again.")
    end
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp redirect_to_edit(conn, flash_level, flash) do
    conn
    |> put_flash(flash_level, flash)
    |> redirect(to: admin_license_path(conn, :edit))
  end
end
