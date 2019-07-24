defmodule IntegrationTest.Acceptance.AdminSettingsTest do
  use IntegrationTest.AcceptanceCase, async: false
  import IntegrationTest.Manager

  test "changing number format" do
    login_as_admin()

    visit_admin_page("Settings")
    fill_field({:css, "#settings_decimal_sep"}, ",")
    fill_field({:css, "#settings_thousand_sep"}, ".")
    fill_field({:css, "#settings_decimal_digits"}, "5")
    click({:xpath, "//button[text()='Save']"})

    assert_has(:xpath, "//*[text()='The settings were saved.']")
    assert_has(:xpath, "//b[text()='123.456.789,12346']")

    visit_data_source(data_source_name())
    start_query("select 1000000.1 from users")
    assert_has(:css, ".panel-success")
    assert_has(:xpath, ".//td[text()='1.000.000,10000']")
  after
    visit_admin_page("Settings")
    fill_field({:css, "#settings_decimal_sep"}, ".")
    fill_field({:css, "#settings_thousand_sep"}, " ")
    fill_field({:css, "#settings_decimal_digits"}, "3")
    click({:xpath, "//button[text()='Save']"})
  end
end
