defmodule Air.Admin.CentralControllerTest do
  use Air.ConnCase, async: false

  import Air.{TestRepoHelper, TestConnHelper}

  setup do
    Application.put_env(:air, :auto_aircloak_export, false)
    on_exit(fn -> Application.put_env(:air, :auto_aircloak_export, true) end)
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

  test "exporting workflow" do
    # generate a pending central call
    Air.Service.Central.record_query(%{})

    # go to the export page and verify that the export link is visible
    initial_conn = login(create_admin_user!()) |> get("/admin/central/export_for_aircloak")
    assert initial_conn.resp_body =~ ~s(href="/admin/central/new_export")

    # "click" on the new export link
    export_conn = initial_conn |> recycle() |> get("/admin/central/new_export")
    assert redirected_to(export_conn) == "/admin/central/export_for_aircloak"
    assert get_flash(export_conn)["info"] =~ "Export generated successfully"

    # fetch generated export data
    last_generated_export = hd(Air.Service.Central.exports(1, 1).entries)
    download_export_path = "/admin/central/download_export/#{last_generated_export.id}"

    # follow redirection and verify that refresh hint is rendered
    redirected_conn = export_conn |> recycle() |> get(redirected_to(export_conn))
    assert response(redirected_conn, 200)
    assert redirected_conn.resp_body =~ ~r(<meta http-equiv="refresh".*url=#{download_export_path}.*)
    refute redirected_conn.resp_body =~ ~s(href="/admin/central/new_export")

    # follow the download refresh link
    downloaded_conn = redirected_conn |> recycle() |> get(download_export_path)
    assert response(downloaded_conn, 200)
    assert hd(get_resp_header(downloaded_conn, "content-disposition")) ==
      "attachment; filename=\"#{Air.Schemas.ExportForAircloak.file_name(last_generated_export)}\""
    assert downloaded_conn.resp_body == last_generated_export.payload

    # verify that reloading of the export page won't trigger another download refresh
    refreshed_conn = redirected_conn |> recycle() |> get("/admin/central/export_for_aircloak")
    assert response(refreshed_conn, 200)
    refute refreshed_conn.resp_body =~ ~r(<meta http-equiv="refresh".*)

    # "click" on the new export link again and verify that it errors
    export_conn = refreshed_conn |> recycle() |> get("/admin/central/new_export")
    assert redirected_to(export_conn) == "/admin/central/export_for_aircloak"
    assert get_flash(export_conn)["error"] =~ "Nothing to export"
  end
end
