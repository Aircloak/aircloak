defmodule Air.AuditLogControllerTest do
  use Air.ConnCase, async: true

  import Air.{TestRepoHelper, TestConnHelper}
  alias Air.TestRepoHelper

  test "can't view audit log as a user" do
    organisation = create_organisation!()
    user = create_user!(organisation)
    assert "/" == login(user) |> get("/audit_log") |> redirected_to()
  end

  test "can view audit log as an admin" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)
    login(admin) |> get("/audit_log") |> response(200)
  end
end
