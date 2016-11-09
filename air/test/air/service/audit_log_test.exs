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
    assert %{"meta" => true} = entry.metadata
  end

  test "filter audit logs by user" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    assert entries_count(%{}, 2)
    assert entries_count(%{users: ["doesnt@exist.com"]}, 0)
    assert entries_count(%{users: [user1.email]}, 1)
    assert entries_count(%{users: [user2.email]}, 1)
    assert entries_count(%{users: [user1.email, user2.email]}, 2)
  end

  test "filter audit logs by event type" do
    user = create_user!()

    AuditLog.log(user, "event1", %{meta: true})
    AuditLog.log(user, "event2", %{meta: true})

    assert entries_count(%{}, 2)
    assert entries_count(%{events: ["missing event"]}, 0)
    assert entries_count(%{events: ["event1"]}, 1)
    assert entries_count(%{events: ["event2"]}, 1)
    assert entries_count(%{events: ["event1", "event2"]}, 2)
  end

  test "lists all event types" do
    user1 = create_user!()
    user2 = create_user!()
    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})
    assert AuditLog.event_types() |> Enum.sort() == ["event1", "event2"]
  end

  test "lists all event types for a user" do
    user1 = create_user!()
    user2 = create_user!()
    user3 = create_user!()
    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})
    assert AuditLog.event_types([user1.email]) == ["event1"]
    assert AuditLog.event_types([user2.email]) == ["event2"]
    assert AuditLog.event_types([user1.email, user2.email]) == ["event1", "event2"]
    assert AuditLog.event_types([user3.email]) == []
  end

  defp entries_count(params, count) do
    length(AuditLog.for(params).entries) == count
  end
end
