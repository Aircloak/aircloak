defmodule Air.ProfileController.Test do
  use AirWeb.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper
  alias Air.{Repo, Schemas.User}

  setup do
    {:ok, user: TestRepoHelper.create_user!()}
  end

  test "updating own details", %{user: user} do
    changed_email = "foo@bar.baz"

    conn =
      login(user)
      |> put(
        "/profile",
        user: %{
          email: changed_email
        }
      )

    assert redirected_to(conn) == "/profile/edit"
    assert Repo.get!(User, user.id).email == changed_email
  end

  test "cannot update own groups", %{user: user} do
    group = TestRepoHelper.create_group!()

    login(user)
    |> put(
      "/profile",
      user: %{
        groups: [group.id]
      }
    )

    user = User |> Repo.get!(user.id) |> Repo.preload([:groups])
    assert user.groups == []
  end

  test "cannot change password without the old password", %{user: user} do
    old_password_hash = user.hashed_password

    login(user)
    |> put(
      "/profile",
      user: %{
        old_password: "incorrect",
        password: "new password",
        password_confirmation: "new password"
      }
    )

    assert Repo.get!(User, user.id).hashed_password == old_password_hash
  end

  test "can change password with the old password", %{user: user} do
    old_password_hash = user.hashed_password

    login(user)
    |> put(
      "/profile",
      user: %{
        old_password: "password1234",
        password: "new password",
        password_confirmation: "new password"
      }
    )

    assert Repo.get!(User, user.id).hashed_password != old_password_hash
  end
end
