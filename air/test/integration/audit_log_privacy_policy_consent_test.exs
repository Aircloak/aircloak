defmodule AuditLogPrivacyPolicyConsent.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  test "logging in should create an audit log entry" do
    user = create_user!()
    user |> login() |> get("/")
    user = Air.Repo.preload(user, :audit_logs)
    assert length(user.audit_logs) > 0
  end
end
