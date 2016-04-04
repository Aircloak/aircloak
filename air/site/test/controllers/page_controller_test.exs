defmodule Air.PageControllerTest do
  use Air.ConnCase, async: false

  alias Air.{TestConnHelper, TestRepoHelper}

  test "GET / redirects for auth", %{conn: conn} do
    conn = get(conn, "/")
    assert redirected_to(conn) =~ "/auth"
    assert get_flash(conn)["error"] =~ "You must be authenticated to view this page"
  end

  test "normal user visible links" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    html = login(user) |> get("/") |> response(200)
    refute html =~ ~s(href="/users")
    refute html =~ ~s(href="/organisations")
    assert html =~ ~s(form action="/logout")
  end

  test "org_admin visible links" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :org_admin)

    html = login(user) |> get("/") |> response(200)
    assert html =~ ~s(href="/organisations/#{org.id}")
    refute html =~ ~s(href="/organisations")
    assert html =~ ~s(form action="/logout")
  end

  test "admin visible links" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :admin)

    html = login(user) |> get("/") |> response(200)
    assert html =~ ~s(href="/users")
    assert html =~ ~s(href="/organisations")
    assert html =~ ~s(form action="/logout")
  end

  defp login(user),
    do: TestConnHelper.login(conn(), user)
end
