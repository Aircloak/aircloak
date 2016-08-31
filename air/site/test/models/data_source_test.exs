defmodule Air.DataSourceTest do
  use Air.ModelCase, async: true

  alias Air.DataSource

  @valid_attrs %{
    unique_id: "unique_id",
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

  test "validates uniqueness of unique id" do
    Repo.insert!(DataSource.changeset(%DataSource{}, @valid_attrs))
    assert_raise Ecto.InvalidChangesetError,
      fn -> Repo.insert!(DataSource.changeset(%DataSource{}, @valid_attrs)) end
  end

  test "invalid tables json defaults to no tables" do
    data_source = Repo.insert!(
      DataSource.changeset(%DataSource{}, Map.merge(@valid_attrs, %{tables: "[invalid"})))
    assert [] == DataSource.tables(data_source)
  end
end
