defmodule Air.Service.UserTest do
  use Air.SchemaCase, async: false # because of shared mode

  alias Air.{Service.User, TestRepoHelper}

  test "required fields", do:
    assert errors_on(%{}) == [
      email: "can't be blank", name: "can't be blank", password: "can't be blank",
      password_confirmation: "can't be blank"
    ]

  test "validates email address", do:
    assert error_on(:email, "invalid_email") == "has invalid format"

  test "validates password and confirmation", do:
    assert error_on(:password_confirmation, "wrong password") == "does not match confirmation"

  test "requires name to be two or more characters", do:
    assert error_on(:name, "a") == "should be at least 2 character(s)"

  test "requires password to be 4 or more characters", do:
    assert error_on(:password, "123") == "should be at least 4 character(s)"

  test "only update hashed password on password change" do
    user = TestRepoHelper.create_user!()

    {:ok, updated_user} = User.update_profile(user, %{"name" => "foobar"})
    assert updated_user.hashed_password == user.hashed_password

    {:ok, updated_user} = User.update_profile(user,
      %{"old_password" => "1234", "password" => "wxyz", "password_confirmation" => "wxyz"})
    refute updated_user.hashed_password == user.hashed_password
  end

  test "the only admin can't be deleted", do:
    assert User.delete(TestRepoHelper.create_only_user_as_admin!()) == {:error, :forbidden_last_admin_deletion}

  test "the only admin can't be updated to be a normal user", do:
    assert User.update(TestRepoHelper.create_only_user_as_admin!(), %{groups: []}) ==
      {:error, :forbidden_last_admin_deletion}

  defp error_on(field, value), do:
    errors_on(%{field => value})[field]

  defp errors_on(changes) do
    assert {:error, changeset} = User.create(changes)

    changeset
    |> Ecto.Changeset.traverse_errors(&Air.ErrorHelpers.translate_error/1)
    |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
  end
end
