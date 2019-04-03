defmodule IntegrationTest.Acceptance.QueryTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "running a query" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> click(Query.xpath("//button[text()='Run']"))
    |> assert_has(Query.css(".panel-success"))
    |> assert_has(Query.xpath(".//td[text()='users']"))
  end

  test "running a query with a keyboard shortcut" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> send_keys(Query.xpath("//*[@id='sql-editor']"), [:control, :enter])
    |> assert_has(Query.css(".panel-success"))
    |> assert_has(Query.xpath(".//td[text()='users']"))
  end

  test "cancelling a query" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> click(Query.xpath("//button[text()='Run']"))
    |> click(Query.xpath("//a[text()='Cancel']"))
    |> find(Query.xpath("//*[text()='Query cancelled']"))
  end
end
