defmodule IntegrationTest.AcceptanceHelper do
  use Wallaby.DSL
  import ExUnit.Assertions

  def new_group_name(), do: "group_#{:erlang.unique_integer([:positive, :monotonic])}"

  def add_group(session, name) do
    session
    |> visit("/admin/groups")
    |> click(Query.css("a", text: "Add a group"))
    |> fill_in(Query.css("#group_name"), with: name)
    |> click(Query.css("button[type='submit']"))
  end

  def login_as_admin(), do: login("admin@aircloak.com", "password1234")

  def login(login, password) do
    new_session()
    |> visit("/auth")
    |> fill_in(Query.css("[name='login']"), with: login)
    |> fill_in(Query.css("[name='password']"), with: password)
    |> click(Query.css("[name='remember']"))
    |> click(Query.css("form button"))
  end

  def new_session() do
    {:ok, session} = Wallaby.start_session()
    resize_window(session, 1920, 1080)
  end

  def accept_confirm!(session, fun) do
    message = accept_confirm(session, fun)
    assert message != nil
    session
  end

  def create_user() do
    name = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)
    login = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)
    password = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

    password_reset_url =
      login_as_admin()
      |> visit("/admin/users")
      |> click(Query.xpath("//a[text()='Add a user']"))
      |> fill_in(Query.xpath("//input[@id='user_login']"), with: login)
      |> fill_in(Query.xpath("//input[@id='user_name']"), with: name)
      |> click(Query.xpath("//button[text()='Save']"))
      |> click(Query.xpath("//a[text()='Reset password']"))
      |> text(Query.xpath("//*[@id='reset-link']"))

    new_session()
    |> visit(password_reset_url)
    |> fill_in(Query.xpath("//input[@id='user_password']"), with: password)
    |> fill_in(Query.xpath("//input[@id='user_password_confirmation']"), with: password)
    |> click(Query.xpath("//button[text()='Save']"))

    %{name: name, login: login, password: password}
  end

  def query_data_source(session, name, query) do
    session
    |> click(Query.xpath("//nav//a[text()='Data sources']"))
    |> click(Query.xpath("//a[text()='#{name}']"))
    |> execute_script("window.codeMirror.editor.setValue('#{query}')")
  end
end
