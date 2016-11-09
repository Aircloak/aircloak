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

  test "filter audit logs by data source" do
    user = create_user!()
    data_source = create_data_source!()

    AuditLog.log(user, "event", %{data_source: data_source.id})

    assert entries_count(%{data_sources: [data_source.id]}, 1)
    assert entries_count(%{data_sources: [data_source.id + 1]}, 0)
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
    assert AuditLog.event_types(%{users: [user1.email]}) == ["event1"]
    assert AuditLog.event_types(%{users: [user2.email]}) == ["event2"]
    assert AuditLog.event_types(%{users: [user1.email, user2.email]}) == ["event1", "event2"]
    assert AuditLog.event_types(%{users: [user3.email]}) == []
  end

  test "users for audit logs is not dependent on user filtering" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    emails = [user1, user2]
      |> Enum.sort_by(&(&1.name))
      |> Enum.map(&(&1.email))
    assert AuditLog.users(%{users: [user1.email]})
      |> Enum.map(&(&1.email)) == emails
  end

  test "lists all users for audit logs" do
    user1 = create_user!()
    user2 = create_user!()
    data_source = create_data_source!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true, data_source: data_source.id})

    assert AuditLog.users(%{events: ["event1"]})
      |> Enum.map(&(&1.email)) == [user1.email]
  end

  test "lists all data sources when no filters" do
    data_source1 = create_data_source!()
    data_source2 = create_data_source!()

    user = create_user!()
    AuditLog.log(user, "event", %{data_source: data_source1.id})
    AuditLog.log(user, "event", %{data_source: data_source2.id})

    names = [data_source1.name, data_source2.name] |> Enum.sort()
    assert AuditLog.data_sources() |> Enum.map(&(&1.name)) == names
  end

  test "data source list filters with other filters" do
    data_source1 = create_data_source!()
    data_source2 = create_data_source!()
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{data_source: data_source1.id})
    AuditLog.log(user1, "event2", %{data_source: data_source2.id})

    names = [data_source1.name, data_source2.name] |> Enum.sort()
    assert AuditLog.data_sources(%{users: [user1.email]})
     |> Enum.map(&(&1.name)) == names
    assert AuditLog.data_sources(%{users: [user2.email]}) == []
    assert AuditLog.data_sources(%{users: [user1.email], events: ["event1"]})
     |> Enum.map(&(&1.name)) == [data_source1.name]
  end

  defp entries_count(params, count) do
    length(AuditLog.for(params).entries) == count
  end
end
