defmodule IntegrationTest.Acceptance.ProfileTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "allows resetting a password" do
    user = create_user()
    new_password = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

    login(user.login, user.password)
    visit_profile_page()
    fill_field({:xpath, "//input[@id='user_old_password']"}, user.password)
    fill_field({:xpath, "//input[@id='user_password']"}, new_password)
    fill_field({:xpath, "//input[@id='user_password_confirmation']"}, new_password)
    click({:xpath, "//form[@action='/profile/change_password']//button[text()='Save']"})

    in_another_session(fn ->
      login(user.login, new_password)
      assert_has(:xpath, "//a[text()='Sign out']")
    end)
  end
end
