defmodule AirWeb.Onboarding.UserController.Test do
  use AirWeb.ConnCase, async: true

  alias Air.Schemas.{User, Group}

  @valid_params %{user: %{
      email: "admin@admin.com",
      name: "admin",
      password: "1234",
      password_confirmation: "1234",
      master_password: "super_secret_master_password",
    }
  }

  setup do
    # Ugly, but needed because:
    # a) sometimes there are user's in the DB already
    # b) because of foreign key constraints in the join table,
    #    we can't just issue a `Repo.delete_all/1`
    Repo.all(User) |> Enum.each(&Repo.delete(&1))
    Repo.all(Group) |> Enum.each(&Repo.delete(&1))
    :ok
  end

  test "creating an admin user", %{conn: conn} do
    post(conn, "/onboarding/", @valid_params)
    [user | _] = users = Repo.all(User) |> Repo.preload([:groups])
    assert length(users) == 1
    assert Enum.any?(user.groups, &(&1.admin))
  end

  test "re-uses admin group if it exists", %{conn: conn} do
    group = Air.TestRepoHelper.create_group!(%{name: "admin", admin: true})
    post(conn, "/onboarding/", @valid_params)
    [user | _] = Repo.all(User) |> Repo.preload([:groups])
    assert hd(user.groups).id == group.id
  end

  test "redirects to users admin page", %{conn: conn} do
    assert post(conn, onboarding_user_path(conn, :create), @valid_params) |> redirected_to() === "/admin/users"
  end

  test "requires correct master password", %{conn: conn} do
    invalid_params = %{@valid_params | user: %{@valid_params[:user] | master_password: "WRONG PASSWORD"}}
    html = conn
    |> post("/onboarding/", invalid_params)
    |> response(200)
    assert html =~ "The master password is incorrect"
  end

  test "instructs user about setup done if has been done in past", %{conn: conn} do
    group = Air.TestRepoHelper.create_group!(%{name: "admin", admin: true})

    Air.TestRepoHelper.create_user!(%{
      name: "test",
      email: "test@example.com",
      password: "1234",
      password_confirmation: "1234",
      groups: [group.id],
    })

    assert get(conn, "/onboarding/") |> redirected_to() === "/onboarding/already_setup"
  end
end
