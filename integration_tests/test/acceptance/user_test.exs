defmodule IntegrationTest.Acceptance.UserTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "adding a user" do
    login = random_string()
    name = random_string()

    create_user(login, name)
    |> assert_has(Query.xpath("//*[text()='User created']"))
    |> visit("/admin/users")
    |> assert_has(Query.xpath("//*[text()='#{name}']"))
    |> assert_has(Query.xpath("//*[text()='#{login}']"))
  end

  test "errors adding a user" do
    create_user("", "")
    |> assert_has(Query.xpath("//*[text()='Oops, something went wrong! Please check the errors below.']"))
    |> assert_has(empty_error("user_login"))
    |> assert_has(empty_error("user_name"))
  end

  test "removing a user" do
    user = create_user()

    login_as_admin()
    |> visit("/admin/users")
    |> accept_confirm!(&click(&1, user_button(user.name, "Permanently delete")))
    |> assert_has(Query.xpath("//*[contains(text(), 'The user has been disabled')]"))
    |> visit("/admin/users")
    |> refute_has(Query.xpath("//*[text()='#{user.name}']"))
    |> refute_has(Query.xpath("//*[text()='#{user.login}']"))
  end

  test "disabling and enabling a user" do
    user = create_user()

    login_as_admin()
    |> visit("/admin/users")
    |> click(user_button(user.name, "Disable"))
    |> assert_has(disabled_user(user.name))
    |> click(user_button(user.name, "Enable"))
    |> visit("/admin/users")
    |> refute_has(disabled_user(user.name))
  end

  defp empty_error(input_id),
    do: Query.xpath(~s{//*[@class='form-group' and descendant::input[@id='#{input_id}']]//*[text()="can't be blank"]})

  defp user_button(user_name, caption), do: Query.xpath("//tr[td[text()='#{user_name}']]//a[text()='#{caption}']")

  defp disabled_user(user_name),
    do: Query.xpath("//div[h3[text()='Disabled user accounts']]/table//td[text()='#{user_name}']")
end
