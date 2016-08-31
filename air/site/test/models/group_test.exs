defmodule Air.GroupTest do
  use Air.ModelCase

  alias Air.Group

  @valid_attrs %{name: "group name"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Group.changeset(%Group{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Group.changeset(%Group{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "validates uniqueness of name" do
    Repo.delete_all(Group)
    Repo.insert!(Group.changeset(%Group{}, @valid_attrs))
    assert_raise Ecto.InvalidChangesetError,
      fn -> Repo.insert!(Group.changeset(%Group{}, @valid_attrs)) end
  end
end
