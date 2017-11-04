defmodule AirWeb.Admin.AuditLogControllerTest do
  use AirWeb.ConnCase, async: true

  import Air.{TestRepoHelper, TestConnHelper}

  test "can't view audit log as a user" do
    assert "/" == login(create_user!()) |> get("/admin/audit_log") |> redirected_to()
  end

  test "can view audit log as an admin" do
    login(create_admin_user!()) |> get("/admin/audit_log") |> response(200)
  end
end
