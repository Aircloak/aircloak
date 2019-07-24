defmodule IntegrationTest.Acceptance.UserTest do
  use IntegrationTest.AcceptanceCase, async: true

  setup do
    login_as_admin()
    :ok
  end

  test "adding a user" do
    login = random_string()
    name = random_string()
    perform_user_creation(login, name)

    assert_has(:xpath, "//*[text()='User created']")
    visit_admin_page("Users")
    assert_has(:xpath, "//*[text()='#{name}']")
    assert_has(:xpath, "//*[text()='#{login}']")
  end

  test "errors adding a user" do
    perform_user_creation("", "")

    assert_has(:xpath, "//*[text()='Oops, something went wrong! Please check the errors below.']")
    assert_has(:xpath, empty_error("user_login"))
    assert_has(:xpath, empty_error("user_name"))
  end

  test "removing a user" do
    user = create_user()

    visit_admin_page("Users")
    click(user_button(user.name, "Permanently delete"))
    accept_dialog()
    assert_has(:xpath, "//*[contains(text(), 'The user has been disabled')]")

    visit_admin_page("Users")
    refute_has(:xpath, "//*[text()='#{user.name}']")
    refute_has(:xpath, "//*[text()='#{user.login}']")
  end

  test "disabling and enabling a user" do
    user = create_user()

    visit_admin_page("Users")
    click(user_button(user.name, "Disable"))
    assert_has(:xpath, disabled_user(user.name))

    click(user_button(user.name, "Enable"))
    refute_has(:xpath, disabled_user(user.name))
  end

  def perform_user_creation(login, name) do
    visit_admin_page("Users")
    click({:xpath, "//a[text()='Add a user']"})
    fill_field({:xpath, "//input[@id='user_login']"}, login)
    fill_field({:xpath, "//input[@id='user_name']"}, name)
    click({:xpath, "//button[text()='Save']"})
  end

  defp empty_error(input_id),
    do: ~s{//*[@class='form-group' and descendant::input[@id='#{input_id}']]//*[text()="can't be blank"]}

  defp user_button(user_name, caption), do: {:xpath, "//tr[td[text()='#{user_name}']]//a[text()='#{caption}']"}

  defp disabled_user(user_name),
    do: "//h3[text()='Disabled user accounts']/following-sibling::table[1]//td[text()='#{user_name}']"
end
