defmodule Air.Schemas.License.Test do
  use Air.SchemaCase, async: true

  alias Air.Schemas.License

  @valid_attrs %{
    text: "Some text"
  }

  test "changeset with valid attributes" do
    changeset = License.changeset(%License{}, @valid_attrs)
    assert changeset.valid?
  end

  test "requires text to be present" do
    changeset = License.changeset(%License{}, %{@valid_attrs | text: nil})
    refute changeset.valid?
  end
end
