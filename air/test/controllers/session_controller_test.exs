defmodule AirWeb.SessionControllerTest do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}

  setup do
    {:ok, user: create_user!()}
  end

  test "anonymous user can access the login page", %{conn: conn} do
    perform_onboarding()
    conn |> get("/auth") |> response(200)
  end

  test "logging in/out", %{user: user} do
    # invalid e-mail
    html = build_conn() |> post("/auth", login: "foo@aircloak.com", password: "password1234") |> response(200)

    assert html =~ "Invalid e-mail or password"

    # invalid password
    html = build_conn() |> post("/auth", login: user.login, password: "") |> response(200)
    assert html =~ "Invalid e-mail or password"

    # correct login
    logged_in_conn = build_conn() |> post("/auth", login: user.login, password: "password1234")
    assert "/" == redirected_to(logged_in_conn)
    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    # verify that the user can now access a page requiring authentication
    recycle(logged_in_conn) |> get("/data_sources") |> response(200)

    # verify that we can now logout
    logged_out_conn = recycle(logged_in_conn) |> delete("/logout")
    assert "/auth" == redirected_to(logged_out_conn)
    assert get_flash(logged_out_conn)["info"] =~ "Logged out successfully"
  end

  test "logged in user can't log in", %{user: user} do
    response = login(user) |> get("/auth") |> response(302)
    assert response =~ ~s(href="/")

    response = login(user) |> post("/auth", login: "", password: "") |> response(302)
    assert response =~ ~s(href="/")
  end

  test "anonymous user can't log out", %{conn: conn} do
    conn = conn |> delete("/logout")
    assert "/auth" == redirected_to(conn)
    assert get_flash(conn)["error"] =~ "You must be authenticated to view this page"
  end

  test "a deleted user is redirected to login", %{user: user} do
    perform_onboarding()
    conn = login(user)
    Air.Repo.delete(user)

    conn = get(conn, "/")
    assert "/auth" == redirected_to(conn)
  end

  test "a disabled user is redirected to login", %{user: user} do
    perform_onboarding()
    conn = login(user)
    {:ok, _} = Air.Service.User.disable(user)

    conn = get(conn, "/")
    assert "/auth" == redirected_to(conn)
  end

  defp perform_onboarding(), do: create_admin_user!()
end
