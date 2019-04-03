defmodule IntegrationTest.Acceptance.ProfileTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "allows resetting a password" do
    user = create_user()
    new_password = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

    new_session()
    |> login(user.login, user.password)
    |> visit("/profile/edit")
    |> fill_in(Query.xpath("//input[@id='user_old_password']"), with: user.password)
    |> fill_in(Query.xpath("//input[@id='user_password']"), with: new_password)
    |> fill_in(Query.xpath("//input[@id='user_password_confirmation']"), with: new_password)
    |> click(Query.xpath("//form[@action='/profile/change_password']//button[text()='Save']"))

    new_session()
    |> login(user.login, new_password)
    |> assert_has(Query.xpath("//a[text()='Sign out']"))
  end

  defp create_user() do
    name = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)
    login = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)
    password = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

    password_reset_url =
      new_session()
      |> login_as_admin()
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
end
