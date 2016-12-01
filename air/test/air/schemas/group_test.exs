defmodule Air.Schemas.GroupTest do
  use Air.ModelCase, async: true

  alias Air.{Schemas.Group, Schemas.User, Schemas.DataSource, TestRepoHelper, TestUtils}

  @valid_attrs %{name: "group name", admin: false}
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
    Repo.insert!(Group.changeset(%Group{}, @valid_attrs))
    assert_raise Ecto.InvalidChangesetError,
      fn -> Repo.insert!(Group.changeset(%Group{}, @valid_attrs)) end
  end

  test "a group can have many users" do
    user1 = TestRepoHelper.create_user!()
    user2 = TestRepoHelper.create_user!()
    group = TestRepoHelper.create_group!()
    |> set_users([user1, user2])
    assert [user1.id, user2.id] == Enum.map(group.users, &(&1.id)) |> Enum.sort()
  end

  test "a group can have many data sources" do
    data_source1 = TestRepoHelper.create_data_source!()
    data_source2 = TestRepoHelper.create_data_source!()
    group = TestRepoHelper.create_group!()
    |> set_data_sources([data_source1, data_source2])
    assert [data_source1.id, data_source2.id] == Enum.map(group.data_sources, &(&1.id)) |> Enum.sort()
  end

  test "deleting a group doesn't delete users or data sources" do
    user = TestRepoHelper.create_user!()
    data_source = TestRepoHelper.create_data_source!()
    TestRepoHelper.create_group!()
    |> set_users([user])
    |> set_data_sources([data_source])
    |> Repo.delete()
    refute nil == Repo.get(User, user.id)
    refute nil == Repo.get(DataSource, data_source.id)
  end

  test "deleting a group, should remove entries from join table" do
    user = TestRepoHelper.create_user!()
    data_source = TestRepoHelper.create_data_source!()
    group = TestRepoHelper.create_group!()
    |> set_users([user])
    |> set_data_sources([data_source])
    TestUtils.assert_join_table_count_change(-2, fn() -> Repo.delete(group) end)
  end

  test "groups, users, and data sources are joined through a join table" do
    user = TestRepoHelper.create_user!()
    data_source = TestRepoHelper.create_data_source!()
    TestUtils.assert_join_table_count_change(2, fn() ->
      TestRepoHelper.create_group!()
      |> set_users([user])
      |> set_data_sources([data_source])
    end)
  end

  test "replacing a user or data source for a group, removes the old relationship" do
    user1 = TestRepoHelper.create_user!()
    user2 = TestRepoHelper.create_user!()
    data_source1 = TestRepoHelper.create_data_source!()
    data_source2 = TestRepoHelper.create_data_source!()

    group = TestRepoHelper.create_group!()
    |> set_users([user1])
    |> set_data_sources([data_source1])

    assert [group.id] == group_ids_from_entity(DataSource, data_source1.id)
    assert [group.id] == group_ids_from_entity(User, user1.id)

    group = TestUtils.assert_join_table_count_change(0, fn() ->
      group
      |> set_users([user2])
      |> set_data_sources([data_source2])
    end)

    assert [] == group_ids_from_entity(DataSource, data_source1.id)
    assert [] == group_ids_from_entity(User, user1.id)
    assert [group.id] == group_ids_from_entity(DataSource, data_source2.id)
    assert [group.id] == group_ids_from_entity(User, user2.id)
  end

  defp group_ids_from_entity(entity, id) do
    entity = Repo.get(entity, id)
    |> Repo.preload(:groups)
    Enum.map(entity.groups, &(&1.id))
  end

  defp set_data_sources(group, data_sources) do
    set_what(group, :data_sources, data_sources)
  end

  defp set_users(group, users) do
    set_what(group, :users, users)
  end

  defp set_what(group, type, items) do
    Repo.get(Group, group.id)
    |> Repo.preload(type)
    |> Group.changeset()
    |> put_assoc(type, items)
    |> Repo.update!()
    Repo.get(Group, group.id)
    |> Repo.preload(type)
  end
end
