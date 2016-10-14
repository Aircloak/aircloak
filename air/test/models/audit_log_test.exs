defmodule Air.AuditLogTest do
  use Air.ModelCase, async: true

  alias Air.AuditLog

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
end
