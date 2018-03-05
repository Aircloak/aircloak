defmodule Central.License.Test do
  use Central.ModelCase, async: true

  alias Central.Schemas.License

  @valid_attrs %{
    name: "Some name",
    length_in_days: 12,
    auto_renew: true,
  }
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = License.changeset(%License{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = License.changeset(%License{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "requires length > 0" do
    attributes = %{@valid_attrs | length_in_days: 0}
    assert errors_on(%License{}, :length_in_days, attributes)
  end
end
