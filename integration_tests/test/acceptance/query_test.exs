defmodule IntegrationTest.Acceptance.QueryTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "running a query" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> click(xpath("//button[text()='Run']"))
    |> assert_has(css(".panel-success"))
    |> assert_has(xpath(".//td[text()='users']"))
  end

  test "running a query with a keyboard shortcut" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> send_keys(xpath("//*[@id='sql-editor']"), [:control, :enter])
    |> assert_has(css(".panel-success"))
    |> assert_has(xpath(".//td[text()='users']"))
  end

  test "cancelling a query" do
    login_as_admin()
    |> query_data_source(IntegrationTest.Manager.data_source_name(), "show tables")
    |> click(xpath("//button[text()='Run']"))
    |> click(xpath("//a[text()='Cancel']"))
    |> find(xpath("//*[text()='Query cancelled']"))
  end
end
