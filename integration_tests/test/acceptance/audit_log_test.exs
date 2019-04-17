defmodule IntegrationTest.Acceptance.AuditLogTest do
  use IntegrationTest.AcceptanceCase, async: false

  setup do
    Air.Repo.delete_all(Air.Schemas.AuditLog)
    :ok
  end

  test "shows events" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("show tables")

    visit_admin_page("Audit log")
    assert_has(:xpath, "//td[text()='Logged in']/following-sibling::td[text()='admin@aircloak.com']")
    assert_has(:xpath, "//td[text()='Executed query']/following-sibling::td[text()='admin@aircloak.com']")

    click({:xpath, "//td[text()='Executed query']/following-sibling::td/a[text()='Events']"})
    click({:xpath, "//td[text()='Executed query']/../..//a[text()='Details']"})
    assert_has(:xpath, "//*[text()='show tables']")
  end

  test "filtering by date" do
    login_as_admin()
    visit_admin_page("Audit log")

    fill_field({:xpath, "//*[contains(text(), 'Filter by time')]/..//input[@id='from']"}, "2001-01-01 00:00:00")
    fill_field({:xpath, "//*[contains(text(), 'Filter by time')]/..//input[@id='to']"}, "2001-02-01 00:00:00")
    click({:xpath, "//*[contains(text(), 'Filter by time')]"})
    click({:xpath, "//*[contains(text(), 'Filter by time')]/..//button[text()='Filter']"})
    assert_has(:xpath, "//td[text()='There are no audit log entries for the current set of filters.']")
  end

  test "filtering by user" do
    another_admin = IntegrationTest.Manager.create_admin_user()
    in_another_session(fn -> login(hd(another_admin.logins).login, IntegrationTest.Manager.user_password()) end)

    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("show tables")

    visit_admin_page("Audit log")

    click({:xpath, "//*[contains(text(), 'Filter by users')]/..//a[text()='aircloak_admin']"})
    refute_has(:xpath, "//td[text()='Logged in']/following-sibling::td[text()='#{hd(another_admin.logins).login}']")

    click({:xpath, "//*[contains(text(), 'Filter by users')]/..//a[contains(text(), 'aircloak_admin')]"})
    assert_has(:xpath, "//td[text()='Logged in']/following-sibling::td[text()='#{hd(another_admin.logins).login}']")
  end

  test "filtering by event type" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("show tables")

    visit_admin_page("Audit log")

    click({:xpath, "//*[contains(text(), 'Filter by event type')]/..//a[text()='Executed query']"})
    refute_has(:xpath, "//td[text()='Logged in']")
    assert_has(:xpath, "//td[text()='Executed query']/following-sibling::td[text()='admin@aircloak.com']")

    click({:xpath, "//*[contains(text(), 'Filter by event type')]/..//a[contains(text(), 'Executed query')]"})
    assert_has(:xpath, "//td[text()='Logged in']")
  end
end
