defmodule Air.UserTest do
  use Air.ModelCase, async: true

  alias Air.User

  @valid_attrs %{email: "admin@aircloak.com", password: "1234", password_confirmation: "1234", name: "Admin"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = User.changeset(%User{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = User.changeset(%User{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "validates email address" do
    attributes = %{@valid_attrs | email: "invalid_email"}
    assert errors_on(%User{}, :email, attributes)
  end

  test "validates password and confirmation" do
    attributes = %{@valid_attrs | password_confirmation: "wrong password"}
    assert errors_on(%User{}, :password_confirmation, attributes)
  end

  test "requires name to be two or more characters" do
    attributes = %{@valid_attrs | name: ""}
    assert errors_on(%User{}, :name, attributes)
  end

  test "only update hashed password on password change" do
    initial_changeset = Map.merge(%User{}, @valid_attrs)
    has_change_fn = fn(attr) ->
      %Ecto.Changeset{changes: changes} = User.changeset(initial_changeset, attr)
      Map.has_key?(changes, :hashed_password)
    end
    # We are simulating the user not editing the password text input boxes
    without_password_change = Map.drop(@valid_attrs, [:password, :password_confirmation])
    refute has_change_fn.(without_password_change)
    changed_password = %{@valid_attrs | password: "abcd", password_confirmation: "abcd"}
    assert has_change_fn.(changed_password)
  end
end
