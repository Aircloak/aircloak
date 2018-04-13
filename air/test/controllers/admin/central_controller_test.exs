defmodule AirWeb.Admin.CentralController.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestRepoHelper, TestConnHelper}

  setup do
    previous_setting = Application.fetch_env!(:air, :auto_aircloak_export)
    Application.put_env(:air, :auto_aircloak_export, false)
    on_exit(fn -> Application.put_env(:air, :auto_aircloak_export, previous_setting) end)
    :ok
  end

  test "can't view exports as user" do
    user = create_user!()
    create_privacy_policy_and_accept_it!(user)
    assert("/" == login(user) |> get("/admin/central/export_for_aircloak") |> redirected_to())
  end

  test "can't view exports in auto mode" do
    Application.put_env(:air, :auto_aircloak_export, true)
    admin_user = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin_user)

    assert "/admin/users" ==
             login(admin_user)
             |> get("/admin/central/export_for_aircloak")
             |> redirected_to()
  end

  test "can view exports as admin" do
    admin_user = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin_user)

    assert(
      login(admin_user)
      |> get("/admin/central/export_for_aircloak")
      |> response(200)
    )
  end

  test "export form is not visible when there's nothing to export" do
    admin_user = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin_user)
    Air.Service.Central.export_pending_calls()

    refute (login(admin_user) |> get("/admin/central/export_for_aircloak")).resp_body =~
             ~s(action="/admin/central/new_export")
  end

  test "new export doesn't work when there's nothing to export" do
    admin_user = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin_user)
    Air.Service.Central.export_pending_calls()

    conn = login(admin_user) |> post("/admin/central/new_export")
    assert redirected_to(conn) == "/admin/central/export_for_aircloak"
    assert get_flash(conn)["error"] =~ "Nothing to export"
  end
end
