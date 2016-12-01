defmodule Air.Schemas.AuditLogTest do
  use Air.SchemaCase, async: true

  alias Air.Schemas.AuditLog

  @valid_attrs %{event: "some content", metadata: %{value: "some content"}, user: "some content"}
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
