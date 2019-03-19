defmodule BrowserTest do
  use ExUnit.Case, async: false
  use Wallaby.DSL

  describe "login" do
    test "unauthenticated user is redirected to login" do
      session = visit(new_session(), "/")
      assert current_path(session) == "/auth"
      assert_has(session, Query.css(".alert", text: "You must be authenticated to view this page"))
    end

    test "shows a message for incorrect login info" do
      session = login(new_session(), "no.such@person.org", "1234")
      assert current_path(session) == "/auth"
      assert_has(session, Query.css(".alert", text: "Invalid login or password."))
    end

    test "allows login for correct login info" do
      session = login(new_session(), "admin@aircloak.com", "password1234")
      assert current_path(session) == "/data_sources"
      assert_has(session, Query.css("a", text: "Sign out"))
    end

    test "remembers the user" do
      new_session()
      |> login("admin@aircloak.com", "password1234")
      |> set_cookie("_air_key", "")
      |> visit("/")
      |> assert_has(Query.css("a", text: "Sign out"))
    end
  end

  defp login(session, login, password) do
    session
    |> visit("/auth")
    |> fill_in(Query.css("[name='login']"), with: login)
    |> fill_in(Query.css("[name='password']"), with: password)
    |> click(Query.css("[name='remember']"))
    |> click(Query.css("form button"))
  end

  defp new_session() do
    {:ok, session} = Wallaby.start_session()
    resize_window(session, 1920, 1080)
  end
end
