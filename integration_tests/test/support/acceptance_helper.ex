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

  def login_as_admin(session), do: login(session, "admin@aircloak.com", "password1234")

  def login(session, login, password) do
    session
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
end
