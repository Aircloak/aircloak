defmodule Air.Service.AuditLogTest do
  use Air.ModelCase, async: true

  import Air.TestRepoHelper
  alias Air.{Repo, Service.AuditLog}

  test "creating audit log entries should save a db record" do
    user = create_user!()

    assert AuditLog.log(user, "event", %{meta: true}) == :ok
    entry = Repo.one!(Air.AuditLog)

    assert entry.event == "event"
    assert entry.user == user.email
    assert %{"meta" => true} = Poison.decode!(entry.metadata)
  end
end
