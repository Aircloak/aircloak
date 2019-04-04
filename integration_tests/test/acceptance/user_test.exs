defmodule IntegrationTest.Acceptance.UserTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "adding a user" do
    login = random_string()
    name = random_string()

    create_user(login, name)
    |> assert_has(xpath("//*[text()='User created']"))
    |> visit_admin_page("Users")
    |> assert_has(xpath("//*[text()='#{name}']"))
    |> assert_has(xpath("//*[text()='#{login}']"))
  end

  test "errors adding a user" do
    create_user("", "")
    |> assert_has(xpath("//*[text()='Oops, something went wrong! Please check the errors below.']"))
    |> assert_has(empty_error("user_login"))
    |> assert_has(empty_error("user_name"))
  end

  test "removing a user" do
    user = create_user()

    login_as_admin()
    |> visit_admin_page("Users")
    |> accept_confirm!(&click(&1, user_button(user.name, "Permanently delete")))
    |> assert_has(xpath("//*[contains(text(), 'The user has been disabled')]"))
    |> visit_admin_page("Users")
    |> refute_has(xpath("//*[text()='#{user.name}']"))
    |> refute_has(xpath("//*[text()='#{user.login}']"))
  end

  test "disabling and enabling a user" do
    user = create_user()

    login_as_admin()
    |> visit_admin_page("Users")
    |> click(user_button(user.name, "Disable"))
    |> assert_has(disabled_user(user.name))
    |> click(user_button(user.name, "Enable"))
    |> refute_has(disabled_user(user.name))
  end

  defp empty_error(input_id),
    do: xpath(~s{//*[@class='form-group' and descendant::input[@id='#{input_id}']]//*[text()="can't be blank"]})

  defp user_button(user_name, caption), do: xpath("//tr[td[text()='#{user_name}']]//a[text()='#{caption}']")

  defp disabled_user(user_name),
    do: xpath("//h3[text()='Disabled user accounts']/following-sibling::table[1]//td[text()='#{user_name}']")
end
