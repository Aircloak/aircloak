defmodule IntegrationTest.Acceptance.DataSourceTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "running a query" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("show tables")
    assert_has(:css, ".panel-success")
    assert_has(:xpath, ".//td[text()='integers']")
  end

  test "running a query with a keyboard shortcut" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    set_query_text("show tables")
    click({:xpath, "//*[@id='sql-editor']"})
    send_keys([:control, :enter])
    assert_has(:css, ".panel-success")
    assert_has(:xpath, ".//td[text()='integers']")
  end

  test "cancelling a query" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("select a.value + b.value from integers a cross join integers b where a.user_id=b.user_id")
    click({:xpath, "//a[text()='Cancel']"})
    assert_has(:xpath, "//*[text()='Query cancelled']")
  end

  test "invalid query" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    start_query("some_query")
    assert_has(:xpath, "//*[text()='Query failed']")
  end

  test "query history" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())

    start_query("some_query")
    Process.sleep(500)
    start_query("another_query")
    Process.sleep(500)
    start_query("yet_another_query")
    Process.sleep(500)

    visit_data_source(IntegrationTest.Manager.data_source_name())
    click({:xpath, "//button[text()='Load previous queries']"})

    assert_has(:xpath, "//*[text()='some_query']")
    assert_has(:xpath, "//*[text()='another_query']")
    assert_has(:xpath, "//*[text()='yet_another_query']")
  end

  defp start_query(text) do
    set_query_text(text)
    click({:xpath, "//button[text()='Run']"})
  end

  defp set_query_text(text), do: execute_script("window.codeMirror.editor.setValue('#{text}')")
end
