defmodule IntegrationTest.Acceptance.ViewTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Helpers

  test "creating a view and selecting from it" do
    login_as_admin()
    view_name = unique_name(:view)
    create_new_view(view_name, "select * from users")
    assert_has(:xpath, view_row_xpath(view_name))

    start_query("select * from #{view_name}")
    assert_has(:css, ".panel-success")
  end

  test "creating a view with a keyboard shortcut" do
    login_as_admin()
    view_name = unique_name(:view)
    create_new_view(view_name, "select * from users", submit_via_keyboard?: true)
    assert_has(:xpath, view_row_xpath(view_name))
  end

  test "empty fields error when creating a view" do
    login_as_admin()
    create_new_view("", "")
    assert_has(:xpath, ~s{//label[text()='Name']/..//*[text()="can't be blank"]})
    assert_has(:xpath, ~s{//label[text()='Code']/..//*[text()="can't be blank"]})
  end

  test "sql error when creating a view" do
    login_as_admin()
    create_new_view(unique_name(:view), "invalid_query")
    assert_has(:xpath, ~s{//label[text()='Code']/..//*[text()="Expected `select or show` at line 1, column 1."]})
  end

  test "editing a view" do
    login_as_admin()
    original_name = unique_name(:view)
    create_new_view(original_name, "select * from users")

    new_name = unique_name(:view)
    edit_view(original_name, name: new_name)

    assert_has(:xpath, view_row_xpath(new_name))
    refute_has(:xpath, view_row_xpath(original_name))
  end

  test "reported error when a view becomes invalid after updating a dependency view" do
    login_as_admin()
    view1 = unique_name(:view)
    view2 = unique_name(:view)
    create_new_view(view1, "select * from users")
    create_new_view(view2, "select * from #{view1}")
    edit_view(view1, name: unique_name(:view))

    assert_has(:xpath, view_row_xpath(view2))
    assert has_class?({:xpath, view_row_xpath(view2)}, "alert-danger")

    hover({:xpath, view_row_xpath(view2)})

    expected_error =
      "This view is no longer valid. " <>
        "This might be caused by a change in the underlying data source, a dependent analyst table, or a view."

    assert_has(:xpath, "//*[text()='#{expected_error}']")
  end

  test "deleting a view" do
    login_as_admin()
    view_name = unique_name(:view)
    create_new_view(view_name, "select * from users")
    click({:xpath, "#{view_row_xpath(view_name)}/..//a[text()='Delete']"})
    accept_dialog()
    Process.sleep(500)
    refute_has(:xpath, view_row_xpath(view_name))
  end

  defp create_new_view(name, query, opts \\ []) do
    visit_data_source(IntegrationTest.Manager.data_source_name())
    click({:xpath, "//a[text()='New view']"})

    fill_field({:css, "#view_name"}, name)

    click({:css, "#viewEditor"})
    send_text(query)

    if Keyword.get(opts, :submit_via_keyboard?),
      do: send_keys([:control, :enter]),
      else: click({:xpath, "//button[text()='Create']"})
  end

  defp edit_view(name, changes) do
    visit_data_source(IntegrationTest.Manager.data_source_name())

    click({:xpath, "#{view_row_xpath(name)}/..//a[text()='Edit']"})

    with {:ok, new_name} <- Keyword.fetch(changes, :name), do: fill_field({:css, "#view_name"}, new_name)

    with {:ok, new_query} <- Keyword.fetch(changes, :query) do
      click({:css, "#viewEditor"})
      Enum.each(1..100, fn _ -> send_keys(:backspace) end)
      send_text(new_query)
    end

    click({:xpath, "//button[text()='Update']"})
  end

  defp view_row_xpath(name), do: "//div[./div/strong[text()='Tables and views']]//*[text()='#{name}']"
end
