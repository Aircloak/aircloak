defmodule IntegrationTest.Acceptance.OnboardingTest do
  use IntegrationTest.AcceptanceCase, async: false
  alias IntegrationTest.Manager

  test "onboarding" do
    Manager.reset_air()
    create_first_admin()
    upload_license()
    set_privacy_policy()
    add_access_to_data_source()
    query_data_source()
  after
    Manager.reset_air()
    Manager.setup_air_database()
  end

  defp create_first_admin() do
    visit("/")
    fill_field({:css, "#user_master_password"}, "super_secret_master_password")
    fill_field({:css, "#user_login"}, "admin@aircloak.com")
    fill_field({:css, "#user_name"}, "aircloak_admin")
    fill_field({:css, "#user_password"}, "password1234")
    fill_field({:css, "#user_password_confirmation"}, "password1234")
    click({:xpath, "//button[text()='Create and login']"})

    assert_has(:xpath, "//*[contains(text(), 'Logged in successfully.')]")
  end

  defp upload_license() do
    click({:xpath, "//a[text()='Aircloak license']"})
    assert_has(:xpath, ~s{//*[text()="Your system doesn't have a valid license."]})

    # We need to load the license programmatically, since we can't manipulate file upload dialog via hound/selenium.
    Manager.load_valid_license()
    click({:xpath, "//a[text()='Aircloak license']"})
    refute_has(:xpath, "//*[text()='Your system doesn't have a valid license.']")
  end

  defp set_privacy_policy() do
    click({:xpath, "//a[text()='Privacy policy']"})
    assert_has(:xpath, ~s{//*[text()="Set your privacy policy"]})

    click({:css, ".privacy-policy-editor"})
    send_text("some privacy policy")
    click({:xpath, "//button[text()='Save privacy policy']"})

    click({:xpath, "//a[text()='Privacy policy']"})
    refute_has(:xpath, ~s{//*[text()="Set your privacy policy"]})
  end

  defp add_access_to_data_source() do
    visit_admin_page("Groups")
    click({:xpath, "//tr//a[text()='Edit']"})
    click({:xpath, "//tr[.//*[text()='#{Manager.data_source_name()}']]//input[@type='checkbox']"})
    click({:xpath, "//button[text()='Save group']"})
  end

  defp query_data_source() do
    visit_data_source(Manager.data_source_name())
    start_query("show tables")
    assert_has(:css, ".panel-success")
  end
end
