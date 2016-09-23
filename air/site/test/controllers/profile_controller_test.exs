defmodule Air.ProfileController.Test do
  use Air.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper
  alias Air.{Repo, User}

  test "updating own details" do
    user = TestRepoHelper.create_user!()

    changed_email = "foo@bar.baz"

    conn = login(user)
    |> put("/profile", user: %{
      email: changed_email
    })

    assert redirected_to(conn) == "/profile"
    assert Repo.get!(User, user.id).email == changed_email
  end

  test "cannot update own groups" do
    user = TestRepoHelper.create_user!()
    group = TestRepoHelper.create_group!()

    login(user)
    |> put("/profile", user: %{
      groups: [group.id]
    })

    user = User |> Repo.get!(user.id) |> Repo.preload([:groups])
    assert user.groups == []
  end

  test "cannot change password without the old password" do
    user = TestRepoHelper.create_user!()
    old_password_hash = user.hashed_password

    login(user)
    |> put("/profile", user: %{
      old_password: "incorrect",
      password: "new password",
      password_confirmation: "new password"
    })

    assert Repo.get!(User, user.id).hashed_password == old_password_hash
  end

  test "can change password with the old password" do
    user = TestRepoHelper.create_user!()
    old_password_hash = user.hashed_password

    login(user)
    |> put("/profile", user: %{
      old_password: "1234",
      password: "new password",
      password_confirmation: "new password"
    })

    assert Repo.get!(User, user.id).hashed_password != old_password_hash
  end
end
