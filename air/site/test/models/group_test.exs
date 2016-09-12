defmodule Air.GroupTest do
  use Air.ModelCase

  alias Air.{Group, TestRepoHelper}

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

  test "group can have many users" do
    org = TestRepoHelper.create_organisation!()
    user1 = TestRepoHelper.create_user!(org, :user)
    user2 = TestRepoHelper.create_user!(org, :user)
    group = TestRepoHelper.create_group!()
    |> Repo.preload(:users)
    |> Group.changeset()
    |> put_assoc(:users, [user1, user2])
    |> Repo.update!()
    |> Repo.preload(:users)
    assert [user1.id, user2.id] == Enum.map(group.users, &(&1.id)) |> Enum.sort()
  end
end
