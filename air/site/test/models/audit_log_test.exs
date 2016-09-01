defmodule Air.AuditLogTest do
  use Air.ModelCase, async: true

  import Air.{TestRepoHelper}
  alias Air.{AuditLog, Repo}

  @valid_attrs %{event: "some content", metadata: "some content", user: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = AuditLog.changeset(%AuditLog{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = AuditLog.changeset(%AuditLog{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "creating audit log entries should save a db record" do
    organisation = create_organisation!()
    user = create_user!(organisation)
    conn = assign_user(%Plug.Conn{}, user)

    assert AuditLog.log(conn, "event", meta: true) == :ok
    entry = Repo.one!(AuditLog)

    assert entry.event == "event"
    assert entry.user == user.email
    assert %{"meta" => true} = Poison.decode!(entry.metadata)
  end

  test "reads user from connection if none provided" do
    organisation = create_organisation!()
    user = create_user!(organisation)
    conn = assign_user(%Plug.Conn{}, user)

    assert AuditLog.log(conn, "event") == :ok
    entry = Repo.one!(AuditLog)

    assert entry.event == "event"
    assert entry.user == user.email
  end

  test "sets IP and remote_ip in metadata" do
    organisation = create_organisation!()
    user = create_user!(organisation)
    conn = %{%Plug.Conn{} | remote_ip: {127, 0, 0, 1}, peer: {{127, 0, 0, 1}, 1234}}
    conn = assign_user(conn, user)

    assert AuditLog.log(conn, "event") == :ok
    entry = Repo.one!(AuditLog)

    assert entry.event == "event"
    assert entry.user == user.email
    assert Poison.decode!(entry.metadata) == %{"peer" => "127.0.0.1:1234", "remote_ip" => "127.0.0.1"}
  end

  defp assign_user(conn, user), do: Plug.Conn.assign(conn, :current_user, user)
end
