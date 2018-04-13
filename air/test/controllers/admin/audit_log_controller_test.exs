defmodule AirWeb.Admin.AuditLogControllerTest do
  use AirWeb.ConnCase, async: true

  import Air.{TestRepoHelper, TestConnHelper}

  test "can't view audit log as a user" do
    user = create_user!()
    create_privacy_policy_and_accept_it!(user)
    assert "/" == login(user) |> get("/admin/audit_log") |> redirected_to()
  end

  test "can view audit log as an admin" do
    admin = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin)
    login(admin) |> get("/admin/audit_log") |> response(200)
  end
end
