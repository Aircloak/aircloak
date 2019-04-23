defmodule IntegrationTest.Acceptance.CloaksTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Manager

  test "shows the list of cloaks" do
    login_as_admin()
    visit_admin_page("Cloaks")

    cloak_div = find_element(:xpath, "//h1[contains(text(), '#{Cloak.AirSocket.cloak_name()}')]/../..")
    assert_has(cloak_div, :xpath, ".//*[contains(text(), 'This cloak is online')]")

    data_source_row =
      find_within_element(
        cloak_div,
        :xpath,
        ".//td[contains(text(), '#{data_source_name()}')]/.."
      )

    assert_has(data_source_row, :xpath, ".//*[contains(text(), 'Online')]")
  end

  test "clicking on show navigates to the data source page" do
    login_as_admin()
    visit_admin_page("Cloaks")
    click({:xpath, "//td[contains(text(), '#{data_source_name()}')]/..//a[text()='Show']"})
    assert current_path() == "/admin/data_sources/#{data_source_name()}"
  end
end
