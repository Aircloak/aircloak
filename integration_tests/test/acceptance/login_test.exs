defmodule IntegrationTest.Acceptance.LoginTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "unauthenticated user is redirected to login" do
    session = visit(new_session(), "/")
    assert current_path(session) == "/auth"
    assert_has(session, Query.css(".alert", text: "You must be authenticated to view this page"))
  end

  test "shows a message for incorrect login info" do
    session = login("no.such@person.org", "1234")
    assert current_path(session) == "/auth"
    assert_has(session, Query.css(".alert", text: "Invalid login or password."))
  end

  test "allows login for correct login info" do
    session = login_as_admin()
    assert current_path(session) == "/data_sources"
    assert_has(session, Query.css("a", text: "Sign out"))
  end

  test "remembers the user" do
    login_as_admin()
    |> set_cookie("_air_key", "")
    |> visit("/")
    |> assert_has(Query.css("a", text: "Sign out"))
  end
end
