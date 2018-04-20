defmodule ExpiredLicenseTest do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  setup do
    :ok = "priv/expired_dev_license.lic" |> File.read!() |> Air.Service.License.load()
    on_exit(fn -> "priv/dev_license.lic" |> File.read!() |> Air.Service.License.load() end)
    user = create_user!()
    admin = create_admin_user!()
    admin = Air.Service.User.load(admin.id)
    {:ok, user: user, admin: admin}
  end

  describe "trying to run a query" do
    test "shows a splash to regular users", %{user: user} do
      result = user |> login() |> post("/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end

    test "redirects admins to license edit", %{admin: admin} do
      result = admin |> login() |> post("/queries", %{})
      assert response(result, 302) =~ "/admin/license/edit"
    end
  end

  describe "trying to run a query via API" do
    test "shows a message to regular users", %{user: user} do
      result = user |> create_token!() |> api_conn() |> post("/api/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end

    test "shows a message to admins", %{admin: admin} do
      result = admin |> create_token!() |> api_conn() |> post("/api/queries", %{})
      assert response(result, 402) =~ "The license for this Aircloak instance has expired."
    end
  end

  test "admin panel can still be accessed (valid privacy policy)", %{admin: admin} do
    result = admin |> login() |> get("/admin/license/edit")
    assert response(result, 200) =~ "Upload license file"
  end

  test "admin panel can still be accessed (no privacy policy)", %{admin: admin} do
    delete_all_privacy_policies!()
    result = admin |> login() |> get("/admin/license/edit")
    assert response(result, 200) =~ "Upload license file"
  end

  test "admin panel can still be accessed (un-accepted privacy policy)", %{admin: admin} do
    create_privacy_policy!()
    result = admin |> login() |> get("/admin/license/edit")
    assert response(result, 200) =~ "Upload license file"
  end

  test "uploading a license", %{admin: admin} do
    upload = %Plug.Upload{path: "priv/dev_license.lic", filename: "dev_license.lic"}
    result = admin |> login() |> put("/admin/license", %{license: %{text: upload}})

    assert response(result, 302) =~ "/admin/license/edit"
    assert Air.Service.License.valid?()
  end
end
