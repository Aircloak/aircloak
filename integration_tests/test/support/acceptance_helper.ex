defmodule IntegrationTest.AcceptanceHelper do
  use Wallaby.DSL
  import Wallaby.Query, only: [css: 1, css: 2, xpath: 1]
  import ExUnit.Assertions

  def new_group_name(), do: "group_#{:erlang.unique_integer([:positive, :monotonic])}"

  def add_group(session, name) do
    session
    |> visit("/admin/groups")
    |> click(css("a", text: "Add a group"))
    |> fill_in(css("#group_name"), with: name)
    |> click(css("button[type='submit']"))
  end

  def login_as_admin(), do: login("admin@aircloak.com", "password1234")

  def login(login, password) do
    new_session()
    |> visit("/auth")
    |> fill_in(css("[name='login']"), with: login)
    |> fill_in(css("[name='password']"), with: password)
    |> click(css("[name='remember']"))
    |> click(css("form button"))
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
    login = random_string()
    name = random_string()
    password = random_string()

    password_reset_url =
      create_user(login, name)
      |> click(xpath("//a[text()='Reset password']"))
      |> text(xpath("//*[@id='reset-link']"))

    new_session()
    |> visit(password_reset_url)
    |> fill_in(xpath("//input[@id='user_password']"), with: password)
    |> fill_in(xpath("//input[@id='user_password_confirmation']"), with: password)
    |> click(xpath("//button[text()='Save']"))

    %{name: name, login: login, password: password}
  end

  def create_user(login, name) do
    login_as_admin()
    |> visit("/admin/users")
    |> click(xpath("//a[text()='Add a user']"))
    |> fill_in(xpath("//input[@id='user_login']"), with: login)
    |> fill_in(xpath("//input[@id='user_name']"), with: name)
    |> click(xpath("//button[text()='Save']"))
  end

  def query_data_source(session, name, query) do
    session
    |> click(xpath("//nav//a[text()='Data sources']"))
    |> click(xpath("//a[text()='#{name}']"))
    |> execute_script("window.codeMirror.editor.setValue('#{query}')")
  end

  def random_string(), do: :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)
end
