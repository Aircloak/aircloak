defmodule Air.SessionControllerTest do
  use Air.ConnCase, async: false

  alias Air.{TestConnHelper, TestRepoHelper}

  test "anonymous user can access the login page", %{conn: conn} do
    conn |> get("/auth") |> response(200)
  end

  test "logging in/out" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    # invalid e-mail
    html = conn() |> post("/auth", email: "foo@aircloak.com", password: "1234") |> response(200)
    assert html =~ "Invalid e-mail or password"

    # invalid password
    html = conn() |> post("/auth", email: user.email, password: "") |> response(200)
    assert html =~ "Invalid e-mail or password"

    # correct login
    logged_in_conn = conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)
    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    # verify that the user can now access the root page
    recycle(logged_in_conn) |> get("/") |> response(200)

    # verify that we can now logout
    logged_out_conn = recycle(logged_in_conn) |> delete("/logout")
    assert "/auth" == redirected_to(logged_out_conn)
    assert get_flash(logged_out_conn)["info"] =~ "Logged out successfully"
  end

  test "logged in user can't log in" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    response = login(user) |> get("/auth") |> response(400)
    assert response =~ "already authenticated"

    response = login(user) |> post("/auth", email: "", password: "") |> response(400)
    assert response =~ "already authenticated"
  end

  test "anonymous user can't log out", %{conn: conn} do
    conn = conn |> delete("/logout")
    assert "/auth" == redirected_to(conn)
    assert get_flash(conn)["error"] =~ "You must be authenticated to view this page"
  end

  defp login(user),
    do: TestConnHelper.login(conn(), user)
end
