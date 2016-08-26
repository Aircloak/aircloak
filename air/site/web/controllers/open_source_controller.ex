defmodule Air.OpenSourceController do
  @moduledoc false
  use Air.Web, :controller

  def permissions do
    %{user: :all}
  end

  def index(conn, _params) do
    packages = Air.BOM.get(fn(packages) ->
      Enum.map(packages, fn(package) ->
        %{
          name: package["name"],
          type: package["type"],
          license_type: package["license"]["type"],
        }
      end)
    end)

    render(conn, "index.html", packages: packages)
  end
end
