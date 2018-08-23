defmodule Air.Service.AuditLogTest do
  use Air.SchemaCase, async: true

  import Air.TestRepoHelper
  alias Air.{Repo, Service.AuditLog}
  import Ecto.Query

  test "creating audit log entries should save a db record" do
    user = create_user!()

    assert AuditLog.log(user, "event", %{meta: true}) == :ok
    entry = Repo.one!(Air.Schemas.AuditLog)

    assert entry.event == "event"
    assert entry.user_id == user.id
    assert %{"meta" => true} = entry.metadata
  end

  test "filter audit logs by time" do
    user = create_user!()

    AuditLog.log(user, "event1", %{})
    AuditLog.log(user, "event1", %{})
    AuditLog.log(user, "event1", %{})

    [first, _, last] = Repo.all(Air.Schemas.AuditLog |> order_by(:inserted_at))

    from = first.inserted_at |> Timex.shift(milliseconds: 1)
    to = last.inserted_at |> Timex.shift(milliseconds: -1)
    assert entries_count(params(%{from: from, to: to}), 1)
  end

  test "limiting number of results" do
    user = create_user!()
    for _ <- 1..3, do: AuditLog.log(user, "event1", %{})

    assert entries_count(params(%{max_results: 2}), 2)
  end

  test "filter audit logs by user" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    assert entries_count(params(), 2)
    assert entries_count(params(%{users: [-1]}), 0)
    assert entries_count(params(%{users: [user1.id]}), 1)
    assert entries_count(params(%{users: [user2.id]}), 1)
    assert entries_count(params(%{users: [user1.id, user2.id]}), 2)
  end

  test "filter audit logs by data source" do
    user = create_user!()
    data_source = create_data_source!()

    AuditLog.log(user, "event", %{data_source: data_source.name})

    assert entries_count(params(%{data_sources: [data_source.name]}), 1)
    assert entries_count(params(%{data_sources: ["data source that doesn't exist"]}), 0)
  end

  test "filter audit logs by event type" do
    user = create_user!()

    AuditLog.log(user, "event1", %{meta: true})
    AuditLog.log(user, "event2", %{meta: true})

    assert entries_count(params(), 2)
    assert entries_count(params(%{events: ["missing event"]}), 0)
    assert entries_count(params(%{events: ["event1"]}), 1)
    assert entries_count(params(%{events: ["event2"]}), 1)
    assert entries_count(params(%{events: ["event1", "event2"]}), 2)
  end

  test "lists all event types" do
    user1 = create_user!()
    user2 = create_user!()
    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})
    assert AuditLog.event_types(params()) |> Enum.sort() == ["event1", "event2"]
  end

  test "lists all event types for a user" do
    user1 = create_user!()
    user2 = create_user!()
    user3 = create_user!()
    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})
    assert AuditLog.event_types(params(%{users: [user1.id]})) == ["event1"]
    assert AuditLog.event_types(params(%{users: [user2.id]})) == ["event2"]

    assert AuditLog.event_types(params(%{users: [user1.id, user2.id]})) == ["event1", "event2"]

    assert AuditLog.event_types(params(%{users: [user3.id]})) == []
  end

  test "includes selected event types irrespective of filters" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    assert AuditLog.event_types(params(%{users: [user1.id], events: ["event2"]})) == [
             "event1",
             "event2"
           ]
  end

  test "users for audit logs is not dependent on user filtering" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    logins =
      [user1, user2]
      |> Enum.sort_by(& &1.name)
      |> Enum.map(& &1.login)

    assert AuditLog.users(params(%{users: [user1.id]}))
           |> Enum.map(& &1.login) == logins
  end

  test "lists all users for audit logs" do
    user1 = create_user!()
    user2 = create_user!()
    data_source = create_data_source!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true, data_source: data_source.name})

    assert AuditLog.users(params(%{events: ["event1"]}))
           |> Enum.map(& &1.login) == [user1.login]
  end

  test "includes selected users irrespective of filters" do
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{meta: true})
    AuditLog.log(user2, "event2", %{meta: true})

    expected = [user1, user2] |> Enum.sort_by(& &1.name) |> Enum.map(& &1.id)

    assert AuditLog.users(params(%{users: [user1.id], events: ["event2"]})) |> Enum.map(& &1.id) == expected
  end

  test "lists all data sources when no filters" do
    data_source1 = create_data_source!()
    data_source2 = create_data_source!()

    user = create_user!()
    AuditLog.log(user, "event", %{data_source: data_source1.name})
    AuditLog.log(user, "event", %{data_source: data_source2.name})

    names = [data_source1.name, data_source2.name] |> Enum.sort()
    assert AuditLog.data_sources(params()) |> Enum.map(& &1.name) == names
  end

  test "data source list filters with other filters" do
    data_source1 = create_data_source!()
    data_source2 = create_data_source!()
    user1 = create_user!()
    user2 = create_user!()

    AuditLog.log(user1, "event1", %{data_source: data_source1.name})
    AuditLog.log(user1, "event2", %{data_source: data_source2.name})

    names = [data_source1.name, data_source2.name] |> Enum.sort()

    assert AuditLog.data_sources(params(%{users: [user1.id]}))
           |> Enum.map(& &1.name) == names

    assert AuditLog.data_sources(params(%{users: [user2.id]})) == []

    assert AuditLog.data_sources(params(%{users: [user1.id], events: ["event1"]}))
           |> Enum.map(& &1.name) == [data_source1.name]
  end

  test "includes selected data sources irrespecitve of filters" do
    user1 = create_user!()
    user2 = create_user!()
    data_source1 = create_data_source!()
    data_source2 = create_data_source!()
    AuditLog.log(user1, "event1", %{data_source: data_source1.name})
    AuditLog.log(user2, "event2", %{data_source: data_source2.name})

    names = [data_source1.name, data_source2.name] |> Enum.sort()

    assert AuditLog.data_sources(params(%{users: [user1.id], data_sources: [data_source2.name]}))
           |> Enum.map(& &1.name) == names
  end

  defp params(provided \\ %{}) do
    provided
    |> Map.put(:page, Map.get(provided, :page, 1))
    |> Map.put(:users, Map.get(provided, :users, []))
    |> Map.put(:events, Map.get(provided, :events, []))
    |> Map.put(:data_sources, Map.get(provided, :data_sources, []))
    |> Map.put(:from, Map.get(provided, :from, Timex.now() |> Timex.shift(days: -1)))
    |> Map.put(:to, Map.get(provided, :to, Timex.now() |> Timex.shift(days: 1)))
    |> Map.put(:max_results, Map.get(provided, :max_results, 100))
  end

  defp entries_count(params, count) do
    params |> AuditLog.for() |> length() == count
  end
end
