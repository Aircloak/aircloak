defmodule IntegrationTest.Acceptance.LoginTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "unauthenticated user is redirected to login" do
    visit("/")
    assert current_path() == "/auth"
    assert_has(:xpath, "//*[text()='You must be authenticated to view this page']")
  end

  test "shows a message for incorrect login info" do
    login("no.such@person.org", "1234")
    assert current_path() == "/auth"
    assert_has(:xpath, "//*[text()='Invalid login or password.']")
  end

  test "allows login for correct login info" do
    login_as_admin()
    assert_has(:xpath, "//a[text()='Sign out']")
  end

  test "remembers the user" do
    visit("/")
    set_cookie(%{name: "auth_remember_me", value: auth_remember_me_cookie()})
    visit("/")
    assert_has(:xpath, "//*[text()='Sign out']")
  end

  test "logout" do
    login_as_admin()
    click({:xpath, "//a[text()='Sign out']"})
    assert_has(:xpath, "//*[text()='Logged out successfully']")
    refute_has(:xpath, "//*[text()='Sign out']")
  end

  defp auth_remember_me_cookie() do
    in_another_session(fn ->
      login_as_admin(remember_me?: true)
      cookie_value("auth_remember_me")
    end)
  end
end
