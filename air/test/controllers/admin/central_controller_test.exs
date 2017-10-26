defmodule Air.Admin.CentralControllerTest do
  use Air.ConnCase, async: false

  import Air.{TestRepoHelper, TestConnHelper}
  alias Air.Schemas.ExportForAircloak

  setup do
    previous_setting = Application.fetch_env!(:air, :auto_aircloak_export)
    Application.put_env(:air, :auto_aircloak_export, false)
    on_exit(fn -> Application.put_env(:air, :auto_aircloak_export, previous_setting) end)
    :ok
  end

  test "can't view exports as user", do:
    assert "/" == login(create_user!()) |> get("/admin/central/export_for_aircloak") |> redirected_to()

  test "can't view exports in auto mode" do
    Application.put_env(:air, :auto_aircloak_export, true)
    assert "/admin/users" ==
      login(create_admin_user!()) |> get("/admin/central/export_for_aircloak") |> redirected_to()
  end

  test "can view exports as admin", do:
    assert login(create_admin_user!()) |> get("/admin/central/export_for_aircloak") |> response(200)

  test "export form is not visible when there's nothing to export" do
    Air.Service.Central.export_pending_calls()
    refute (login(create_admin_user!()) |> get("/admin/central/export_for_aircloak")).resp_body =~
      ~s(action="/admin/central/new_export")
  end

  test "export form is visible when there's something to export" do
    Air.Service.Central.record_query(%{})
    assert (login(create_admin_user!()) |> get("/admin/central/export_for_aircloak")).resp_body =~
      ~s(data-to="/admin/central/new_export")
  end

  test "generating a new export" do
    Air.Service.Central.record_query(%{})
    conn = login(create_admin_user!()) |> post("/admin/central/new_export")
    assert redirected_to(conn) == "/admin/central/export_for_aircloak"
    assert get_flash(conn)["info"] =~ "Export generated successfully"
  end

  test "new export doesn't work when there's nothing to export" do
    Air.Service.Central.export_pending_calls()
    conn = login(create_admin_user!()) |> post("/admin/central/new_export")
    assert redirected_to(conn) == "/admin/central/export_for_aircloak"
    assert get_flash(conn)["error"] =~ "Nothing to export"
  end

  test "auto download after creating a new export" do
    Air.Service.Central.record_query(%{})

    conn =
      login(create_admin_user!())
      |> post("/admin/central/new_export")
      |> recycle()
      |> get("/admin/central/export_for_aircloak")

    assert conn.resp_body =~ ~r(<meta http-equiv="refresh".*url=#{download_path(last_generated_export())}.*)
    refute conn.resp_body =~ ~s(href="/admin/central/new_export")
  end

  test "auto download starts only once" do
    Air.Service.Central.record_query(%{})

    conn =
      login(create_admin_user!())
      |> post("/admin/central/new_export")
      |> recycle()
      |> get("/admin/central/export_for_aircloak")
      |> recycle()
      |> get("/admin/central/export_for_aircloak")

    refute conn.resp_body =~ ~r(<meta http-equiv="refresh".*url=#{download_path(last_generated_export())}.*)
  end

  test "downloading an export" do
    Air.Service.Central.export_pending_calls()
    Air.Service.Central.record_query(%{})
    {:ok, export} = Air.Service.Central.export_pending_calls()

    conn = login(create_admin_user!()) |> get(download_path(export))

    assert response(conn, 200)
    assert hd(get_resp_header(conn, "content-disposition")) ==
      "attachment; filename=\"#{ExportForAircloak.file_name(export)}\""
    assert conn.resp_body == ExportForAircloak.content(export)
  end


  defp last_generated_export(), do:
    hd(Air.Service.Central.exports(1, 1).entries)

  defp download_path(export), do:
    "/admin/central/download_export/#{export.id}"
end
