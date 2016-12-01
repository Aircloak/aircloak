defmodule Air.Schemas.DataSourceTest do
  use Air.SchemaCase, async: true

  alias Air.{Schemas.DataSource, Schemas.Group, TestRepoHelper, TestUtils}

  @valid_attrs %{
    global_id: "global_id",
    name: "name",
    tables: "[]",
  }
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = DataSource.changeset(%DataSource{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = DataSource.changeset(%DataSource{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "validates uniqueness of global id" do
    Repo.insert!(DataSource.changeset(%DataSource{}, @valid_attrs))
    assert_raise Ecto.InvalidChangesetError,
      fn -> Repo.insert!(DataSource.changeset(%DataSource{}, @valid_attrs)) end
  end

  test "invalid tables json defaults to no tables" do
    data_source = Repo.insert!(
      DataSource.changeset(%DataSource{}, Map.merge(@valid_attrs, %{tables: "[invalid"})))
    assert [] == DataSource.tables(data_source)
  end

  test "a data_source can have many groups" do
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!()
    |> set_groups([group1, group2])
    assert [group1.id, group2.id] == Enum.map(data_source.groups, &(&1.id)) |> Enum.sort()
  end

  test "deleting a data source, doesn't delete the group" do
    group = TestRepoHelper.create_group!()
    TestRepoHelper.create_data_source!()
    |> set_groups([group])
    |> Repo.delete()
    refute nil == Repo.get(Group, group.id)
  end

  test "deleting a data_source, should remove entries from join table" do
    group = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!()
    |> set_groups([group])
    TestUtils.assert_join_table_count_change(-1, fn() -> Repo.delete(data_source) end)
  end

  test "data_sources and groups are joined through a join table" do
    group = TestRepoHelper.create_group!()
    TestUtils.assert_join_table_count_change(1, fn() ->
      TestRepoHelper.create_data_source!()
      |> set_groups([group])
    end)
  end

  test "replacing a group for a data_source, removes the old relationship" do
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!()
    |> set_groups([group1])

    assert [data_source.id] == data_source_ids_from_group(group1)

    TestUtils.assert_join_table_count_change(0, fn() -> set_groups(data_source, [group2]) end)

    assert [] == data_source_ids_from_group(group1)
    assert [data_source.id] == data_source_ids_from_group(group2)
  end

  defp data_source_ids_from_group(group) do
    group = Repo.get(Group, group.id)
    |> Repo.preload(:data_sources)
    Enum.map(group.data_sources, &(&1.id))
  end

  defp set_groups(data_source, groups) do
    Repo.get(DataSource, data_source.id)
    |> Repo.preload(:groups)
    |> DataSource.changeset()
    |> put_assoc(:groups, groups)
    |> Repo.update!()
    # Load the data_source from the database again, in case it will be used further,
    # just to ensure that the stored has the data we need, and that it isn't
    # just in a cached local version.
    Repo.get(DataSource, data_source.id)
    |> Repo.preload(:groups)
  end
end
