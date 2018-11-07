defmodule Air.ProfileController.Test do
  use AirWeb.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper
  alias Air.{Repo, Schemas.User}
  alias Air.Service

  setup do
    {:ok, user: TestRepoHelper.create_user!()}
  end

  test "updating own details", %{user: user} do
    changed_login = "foo@bar.baz"

    conn =
      login(user)
      |> put(
        "/profile",
        user: %{
          login: changed_login
        }
      )

    assert redirected_to(conn) == "/profile/edit"
    assert Service.User.load(user.id) |> Service.User.main_login() == changed_login
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
    old_password_hash = hd(user.logins).hashed_password

    login(user)
    |> put(
      "/profile",
      user: %{
        old_password: "incorrect",
        password: "new password",
        password_confirmation: "new password"
      }
    )

    assert hd(Service.User.load(user.id).logins).hashed_password == old_password_hash
  end

  test "can change password with the old password", %{user: user} do
    old_password_hash = hd(user.logins).hashed_password

    login(user)
    |> put(
      "/profile",
      user: %{
        old_password: "password1234",
        password: "new password",
        password_confirmation: "new password"
      }
    )

    refute hd(Service.User.load(user.id).logins).hashed_password == old_password_hash
  end
end
