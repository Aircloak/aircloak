defmodule IntegrationTest.Acceptance.DataSourceTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "running a query" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    set_query_text("show tables")
    click({:xpath, "//button[text()='Run']"})
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
    set_query_text("select a.value + b.value from integers a cross join integers b where a.user_id=b.user_id")

    click({:xpath, "//button[text()='Run']"})
    click({:xpath, "//a[text()='Cancel']"})
    assert_has(:xpath, "//*[text()='Query cancelled']")
  end

  defp set_query_text(text), do: execute_script("window.codeMirror.editor.setValue('#{text}')")
end
