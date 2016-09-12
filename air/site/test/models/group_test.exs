defmodule Air.GroupTest do
  use Air.ModelCase

  alias Air.{Group, User, DataSource, TestRepoHelper}

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

  test "a group can have many users" do
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

  test "a group can have many data sources" do
    data_source1 = TestRepoHelper.create_data_source!()
    data_source2 = TestRepoHelper.create_data_source!()
    group = TestRepoHelper.create_group!()
    |> Repo.preload(:data_sources)
    |> Group.changeset()
    |> put_assoc(:data_sources, [data_source1, data_source2])
    |> Repo.update!()
    |> Repo.preload(:data_sources)
    assert [data_source1.id, data_source2.id] == Enum.map(group.data_sources, &(&1.id)) |> Enum.sort()
  end

  test "deleting a group doesn't delete users or data sources" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)
    data_source = TestRepoHelper.create_data_source!()
    TestRepoHelper.create_group!()
    |> Repo.preload(:users)
    |> Repo.preload(:data_sources)
    |> Group.changeset()
    |> put_assoc(:users, [user])
    |> put_assoc(:data_sources, [data_source])
    |> Repo.update!()
    |> Repo.delete()
    refute nil == Repo.get(User, user.id)
    refute nil == Repo.get(DataSource, data_source.id)
  end
end
