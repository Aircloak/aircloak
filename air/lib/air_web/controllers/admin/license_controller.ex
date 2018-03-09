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
      user: [:invalid],
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
        :error -> redirect_to_edit(conn, :error, "Could not verify license file")
      end
    else
      _ -> redirect_to_edit(conn, :error, "Unknown error. Please try again.")
    end
  end

  def invalid(conn, _params) do
    if Air.Schemas.User.admin?(conn.assigns.current_user) do
      redirect(conn, to: admin_license_path(conn, :edit))
    else
      render(conn, "invalid.html", layout: false)
    end
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def redirect_to_edit(conn, flash_level, flash) do
    conn
    |> put_flash(flash_level, flash)
    |> redirect(to: admin_license_path(conn, :edit))
  end
end
