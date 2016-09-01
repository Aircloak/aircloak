defmodule Air.OrganisationTest do
  use Air.ModelCase, async: true

  alias Air.Organisation

  @valid_attrs %{name: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Organisation.changeset(%Organisation{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Organisation.changeset(%Organisation{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "unique organisation name" do
    %Organisation{}
    |> Organisation.changeset(%{name: "Test organisation"})
    |> Air.Repo.insert!()

    assert {:error, changeset} = %Organisation{}
    |> Organisation.changeset(%{name: "Test organisation"})
    |> Air.Repo.insert()

    assert [name: {"has already been taken", _}] = changeset.errors
  end
end
