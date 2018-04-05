defmodule AirWeb.Admin.CentralController.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestRepoHelper, TestConnHelper}

  setup do
    previous_setting = Application.fetch_env!(:air, :auto_aircloak_export)
    Application.put_env(:air, :auto_aircloak_export, false)
    on_exit(fn -> Application.put_env(:air, :auto_aircloak_export, previous_setting) end)
    :ok
  end

  test "can't view exports as user",
    do: assert("/" == login(create_user!()) |> get("/admin/central/export_for_aircloak") |> redirected_to())

  test "can't view exports in auto mode" do
    Application.put_env(:air, :auto_aircloak_export, true)

    assert "/admin/users" ==
             login(create_admin_user!())
             |> get("/admin/central/export_for_aircloak")
             |> redirected_to()
  end

  test "can view exports as admin",
    do:
      assert(
        login(create_admin_user!())
        |> get("/admin/central/export_for_aircloak")
        |> response(200)
      )

  test "export form is not visible when there's nothing to export" do
    Air.Service.Central.export_pending_calls()

    refute (login(create_admin_user!()) |> get("/admin/central/export_for_aircloak")).resp_body =~
             ~s(action="/admin/central/new_export")
  end

  test "new export doesn't work when there's nothing to export" do
    Air.Service.Central.export_pending_calls()
    conn = login(create_admin_user!()) |> post("/admin/central/new_export")
    assert redirected_to(conn) == "/admin/central/export_for_aircloak"
    assert get_flash(conn)["error"] =~ "Nothing to export"
  end
end
