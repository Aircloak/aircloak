defmodule Air.Admin.AuditLogControllerTest do
  use Air.ConnCase, async: true

  import Air.{TestRepoHelper, TestConnHelper}
  alias Air.TestRepoHelper

  test "can't view audit log as a user" do
    organisation = create_organisation!()
    user = create_user!(organisation)
    assert "/" == login(user) |> get("/admin/audit_log") |> redirected_to()
  end

  test "can view audit log as an admin" do
    admin = TestRepoHelper.create_admin_user!()
    login(admin) |> get("/admin/audit_log") |> response(200)
  end
end
