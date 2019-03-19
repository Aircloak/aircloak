defmodule BrowserTest do
  use ExUnit.Case, async: false
  use Wallaby.DSL
  alias IntegrationTest.Manager

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
      session = login_as_admin(new_session())
      assert current_path(session) == "/data_sources"
      assert_has(session, Query.css("a", text: "Sign out"))
    end

    test "remembers the user" do
      new_session()
      |> login_as_admin()
      |> set_cookie("_air_key", "")
      |> visit("/")
      |> assert_has(Query.css("a", text: "Sign out"))
    end
  end

  describe "group" do
    test "allows adding a group" do
      name = new_group_name()

      new_session()
      |> login_as_admin()
      |> add_group(name)
      |> assert_has(Query.css("td", text: name))
    end

    test "allows removing a group" do
      name = new_group_name()

      new_session()
      |> login_as_admin()
      |> add_group(name)
      |> accept_confirm!(&click(&1, Query.xpath("//tr[td[text()='#{name}']]//a[text()='Delete']")))
      |> refute_has(Query.css("td", text: name))
    end

    test "forbids access to data source by default" do
      group = Air.Service.User.create_group!(%{name: new_group_name(), admin: false})
      user = Manager.create_air_user(group)

      session =
        new_session()
        |> login_as_admin()
        |> visit("/admin/data_sources")
        |> click(Query.xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//a[text()='Show']"))

      refute_has(session, Query.xpath(".//td[text()='#{user.name}']"))
      refute_has(session, Query.xpath(".//td[text()='#{group.name}']"))
    end

    test "allowing access to a data source through a group" do
      group = Air.Service.User.create_group!(%{name: new_group_name(), admin: false})
      user = Manager.create_air_user(group)

      session =
        new_session()
        |> login_as_admin()
        |> visit("/admin/groups")
        |> click(Query.xpath("//tr[td[text()='#{group.name}']]//a[text()='Edit']"))
        |> click(Query.xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//input[@type='checkbox']"))
        |> click(Query.xpath("//button[text()='Save group']"))
        |> visit("/admin/data_sources")
        |> click(Query.xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//a[text()='Show']"))

      assert_has(session, Query.xpath(".//td[text()='#{user.name}']"))
      assert_has(session, Query.xpath(".//td[text()='#{group.name}']"))
    end
  end

  defp new_group_name(), do: "group_#{:erlang.unique_integer([:positive, :monotonic])}"

  defp add_group(session, name) do
    session
    |> visit("/admin/groups")
    |> click(Query.css("a", text: "Add a group"))
    |> fill_in(Query.css("#group_name"), with: name)
    |> click(Query.css("button[type='submit']"))
  end

  defp login_as_admin(session), do: login(session, "admin@aircloak.com", "password1234")

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

  defp accept_confirm!(session, fun) do
    message = accept_confirm(session, fun)
    assert message != nil
    session
  end
end
