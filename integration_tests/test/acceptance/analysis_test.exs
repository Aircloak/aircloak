defmodule IntegrationTest.Acceptance.AnalysisTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Manager

  test "by data source tab" do
    login_as_admin()
    visit_admin_page("Analysis")

    assert_has(:xpath, "//li[@class='active']/a[text()='By data source']")
    assert_has(:xpath, "//a[text()='#{data_source_name()}']")

    click({:xpath, "//a[text()='#{data_source_name()}']"})
    assert current_path() == "/admin/data_sources/#{data_source_name()}"
  end

  test "by table tab" do
    login_as_admin()
    visit_admin_page("Analysis")

    click({:xpath, "//a[text()='By table']"})
    assert_has(:xpath, "//a[text()='#{data_source_name()}/integers']")
    assert_has(:xpath, "//a[text()='#{data_source_name()}/users']")

    click({:xpath, "//a[text()='#{data_source_name()}/integers']"})
    assert current_path() == "/admin/data_sources/#{data_source_name()}"
  end

  test "by host tab" do
    login_as_admin()
    visit_admin_page("Analysis")

    {:ok, ds} = Cloak.DataSource.fetch(data_source_name())
    click({:xpath, "//a[text()='By host']"})
    assert_has(:xpath, "//*[contains(text(),'#{ds.parameters.hostname}')]")
  end
end
