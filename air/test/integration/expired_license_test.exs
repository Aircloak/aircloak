defmodule ExpiredLicenseTest do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  setup do
    :ok = "priv/expired_dev_license.lic" |> File.read!() |> Air.Service.License.load()
    on_exit(fn -> "priv/dev_license.lic" |> File.read!() |> Air.Service.License.load() end)
    :ok
  end

  describe "trying to run a query" do
    test "shows a splash to regular users" do
      result = create_user!() |> login() |> post("/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end

    test "redirects admins to license edit" do
      result = create_admin_user!() |> login() |> post("/queries", %{})
      assert response(result, 302) =~ "/admin/license/edit"
    end
  end

  describe "trying to run a query via API" do
    test "shows a message to regular users" do
      result = create_user!() |> create_token!() |> api_conn() |> post("/api/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end

    test "shows a message to admins" do
      result = create_admin_user!() |> create_token!() |> api_conn() |> post("/api/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end
  end

  test "admin panel can still be accessed" do
    result = create_admin_user!() |> login() |> get("/admin/license/edit")
    assert response(result, 200) =~ "Upload license file"
  end

  test "uploading a license" do
    upload = %Plug.Upload{path: "priv/dev_license.lic", filename: "dev_license.lic"}
    result = create_admin_user!() |> login() |> put("/admin/license", %{license: %{text: upload}})

    assert response(result, 302) =~ "/admin/license/edit"
    assert Air.Service.License.valid?()
  end
end
