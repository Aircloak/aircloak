defmodule AirWeb.LicenseController do
  @moduledoc false
  use Air.Web, :controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    packages =
      Air.BOM.get(fn packages ->
        Enum.map(packages, fn package ->
          %{
            name: package["name"],
            realm: package["realm"],
            license_type: package["license"]["type"],
            version: package["version"]
          }
        end)
      end)

    render(conn, "index.html", packages: packages)
  end

  def show(conn, params) do
    case Air.BOM.get(fn packages ->
           Enum.find(packages, &(&1["realm"] == params["realm"] and &1["name"] == params["name"]))
         end) do
      nil -> conn |> put_view(AirWeb.ErrorView) |> render("404.html")
      package -> render(conn, "show.html", package: package)
    end
  end

  def dependencies(conn, _params) do
    conn
    |> put_resp_content_type("application/zip")
    |> send_file(200, Air.BOM.dependencies_path())
  end
end
