defmodule IntegrationTest.Acceptance.LoginTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "unauthenticated user is redirected to login" do
    session = visit(new_session(), "/")
    assert_has(session, css(".alert", text: "You must be authenticated to view this page"))
  end

  test "shows a message for incorrect login info" do
    session = login("no.such@person.org", "1234")
    assert_has(session, css(".alert", text: "Invalid login or password."))
  end

  test "allows login for correct login info" do
    session = login_as_admin()
    assert_has(session, css("a", text: "Sign out"))
  end

  test "remembers the user" do
    new_session()
    |> visit("/")
    |> set_cookie("auth_remember_me", auth_remember_me_cookie())
    |> visit("/")
    |> assert_has(css("a", text: "Sign out"))
  end

  defp auth_remember_me_cookie(), do: login_as_admin(remember_me?: true) |> cookie_value("auth_remember_me")
end
