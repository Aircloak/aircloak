defmodule IntegrationTest.Acceptance.ProfileTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "allows resetting a password" do
    user = create_user()
    new_password = :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

    login(user.login, user.password)
    |> visit("/profile/edit")
    |> fill_in(Query.xpath("//input[@id='user_old_password']"), with: user.password)
    |> fill_in(Query.xpath("//input[@id='user_password']"), with: new_password)
    |> fill_in(Query.xpath("//input[@id='user_password_confirmation']"), with: new_password)
    |> click(Query.xpath("//form[@action='/profile/change_password']//button[text()='Save']"))

    assert_has(login(user.login, new_password), Query.xpath("//a[text()='Sign out']"))
  end
end
