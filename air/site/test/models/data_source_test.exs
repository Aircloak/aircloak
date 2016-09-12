defmodule Air.DataSourceTest do
  use Air.ModelCase, async: true

  alias Air.{DataSource, TestRepoHelper}

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
    |> Repo.preload(:groups)
    |> DataSource.changeset()
    |> put_assoc(:groups, [group1, group2])
    |> Repo.update!()
    |> Repo.preload(:groups)
    assert [group1.id, group2.id] == Enum.map(data_source.groups, &(&1.id)) |> Enum.sort()
  end
end
