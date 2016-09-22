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
end
