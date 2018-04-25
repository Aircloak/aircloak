defmodule AuditLogPrivacyPolicyConsent.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  test "logging in should create an audit log entry if the privacy policy has been accepted" do
    user = create_user!()
    user |> login() |> get("/")
    user = Air.Repo.preload(user, :audit_logs)
    assert length(user.audit_logs) > 0
  end

  test "logging in without having accepted the privacy policy should not create audit log entry" do
    user = create_user_without_privacy_policy!()
    create_privacy_policy!()
    user |> login() |> get("/")
    user = Air.Repo.preload(user, :audit_logs)
    assert user.audit_logs == []
  end
end
