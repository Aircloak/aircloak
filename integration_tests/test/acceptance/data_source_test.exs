defmodule IntegrationTest.Acceptance.DataSourceTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "tables panel contains a list of tables" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    tables_panel = find_element(:xpath, ".//div[./div/strong[text()='Tables and views']]")
    assert_has(tables_panel, :xpath, ".//*[text()='integers']")
    assert_has(tables_panel, :xpath, ".//*[text()='users']")
  end

  test "table contains the list of columns" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    user_table_entry = find_element(:xpath, ".//div[./div/strong[text()='Tables and views']]//*[text()='users']/..")
    click(user_table_entry)
    assert_has(user_table_entry, :xpath, ".//td[text()='name']")
    assert_has(user_table_entry, :xpath, ".//td[text()='height']")
  end

  test "filtering the list of columns" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    user_table_entry = find_element(:xpath, ".//div[./div/strong[text()='Tables and views']]//*[text()='users']/..")
    click(user_table_entry)
    fill_field({:xpath, "//input[@placeholder='Filter columns']"}, "heig")
    refute_has(user_table_entry, :xpath, ".//td[text()='name']")
    assert_has(user_table_entry, :xpath, ".//td[text()='height']")
  end

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

  test "sharing the query result publicly" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    link = run_query_and_share_result("show tables", "Public link")

    in_another_session(fn ->
      navigate_to(link)
      assert_has(:xpath, "//td[text()='aircloak_admin']")
      assert_has(:xpath, ".//td[text()='integers']")
      assert_has(:xpath, ".//td[text()='users']")
      refute_has(:xpath, "//a[text()='Share']")
    end)
  end

  test "sharing the query result privately" do
    login_as_admin()
    visit_data_source(IntegrationTest.Manager.data_source_name())
    link = run_query_and_share_result("show tables", "Private link")

    # user with proper permission can visit the link
    in_another_session(fn ->
      another_admin = IntegrationTest.Manager.create_admin_user()
      login(hd(another_admin.logins).login, IntegrationTest.Manager.user_password())
      navigate_to(link)
      assert_has(:xpath, "//td[text()='aircloak_admin']")
      assert_has(:xpath, ".//td[text()='integers']")
      assert_has(:xpath, ".//td[text()='users']")
    end)

    # user must be logged in
    in_another_session(fn ->
      navigate_to(link)
      assert_has(:xpath, "//*[text()='You must be authenticated to view this page']")
    end)

    # user must have proper permissions
    in_another_session(fn ->
      user = create_user()
      login(user.login, user.password)
      navigate_to(link)
      assert_has(:xpath, "//*[text()='The page could not be found']")
    end)
  end

  defp run_query_and_share_result(query, share_caption) do
    start_query(query)
    assert_has(:css, ".panel-success")
    click({:xpath, "//a[text()='Share']"})
    attribute_value({:xpath, "//*[@class='modal-body']//label[text()='#{share_caption}']//../input"}, "value")
  end
end
