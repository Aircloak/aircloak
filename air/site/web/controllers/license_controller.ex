defmodule Air.LicenseController do
  @moduledoc false
  use Air.Web, :controller


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    packages = Air.BOM.get(fn(packages) ->
      Enum.map(packages, fn(package) ->
        %{
          name: package["name"],
          realm: package["realm"],
          license_type: package["license"]["type"],
        }
      end)
    end)

    render(conn, "index.html", packages: packages)
  end

  def show(conn, params) do
    case Air.BOM.get(fn(packages) ->
      Enum.find(packages, &(&1["realm"] == params["realm"] and &1["name"] == params["name"]))
    end) do
      nil -> render(conn, Air.ErrorView, "404.html")
      package -> render(conn, "show.html", package: package)
    end
  end
end
