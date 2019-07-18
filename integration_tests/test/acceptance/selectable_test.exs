defmodule IntegrationTest.Acceptance.ViewTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Helpers

  for selectable_type <- [:view, :table] do
    describe "#{selectable_type}" do
      test "creating and selecting" do
        login_as_admin()
        view_name = unique_name(:view)
        create_new_selectable(unquote(selectable_type), view_name, "select * from users")
        assert_has(:xpath, selectable_row_xpath(view_name))

        start_query("select * from #{view_name}")
        assert_has(:css, ".panel-success")
      end

      test "creating with a keyboard shortcut" do
        login_as_admin()
        view_name = unique_name(:view)
        create_new_selectable(unquote(selectable_type), view_name, "select * from users", submit_via_keyboard?: true)
        assert_has(:xpath, selectable_row_xpath(view_name))
      end

      test "empty fields error when creating" do
        login_as_admin()
        create_new_selectable(unquote(selectable_type), "", "", wait_until_created?: false)
        assert_has(:xpath, ~s{//label[text()='Name']/..//*[text()="can't be blank"]})
        assert_has(:xpath, ~s{//label[text()='Code']/..//*[text()="can't be blank"]})
      end

      test "sql error when creating" do
        login_as_admin()
        create_new_selectable(unquote(selectable_type), unique_name(:view), "invalid_query", wait_until_created?: false)
        assert_has(:xpath, ~s{//label[text()='Code']/..//*[text()="Expected `select or show` at line 1, column 1."]})
      end

      test "editing" do
        login_as_admin()
        original_name = unique_name(:view)
        create_new_selectable(unquote(selectable_type), original_name, "select * from users")

        new_name = unique_name(:view)
        edit_selectable(unquote(selectable_type), original_name, name: new_name)

        assert_has(:xpath, selectable_row_xpath(new_name))
        refute_has(:xpath, selectable_row_xpath(original_name))
      end

      @tag timeout: 15_000
      test "deleting" do
        login_as_admin()
        view_name = unique_name(:view)
        create_new_selectable(unquote(selectable_type), view_name, "select * from users")
        assert_has(nil, :xpath, "#{selectable_row_xpath(view_name)}/..//a[text()='Delete']", attempts: 50)
        click({:xpath, "#{selectable_row_xpath(view_name)}/..//a[text()='Delete']"})
        accept_dialog()
        refute_has(nil, :xpath, selectable_row_xpath(view_name), attempts: 10)
      end
    end
  end

  test "reported error when a view becomes invalid after updating a dependency view" do
    login_as_admin()
    view1 = unique_name(:view)
    view2 = unique_name(:view)
    create_new_selectable(:view, view1, "select * from users")
    create_new_selectable(:view, view2, "select * from #{view1}")
    edit_selectable(:view, view1, name: unique_name(:view))

    assert_has(:xpath, selectable_row_xpath(view2))
    assert has_class?({:xpath, selectable_row_xpath(view2)}, "alert-danger")

    hover({:xpath, selectable_row_xpath(view2)})

    expected_error =
      "This view is no longer valid. " <>
        "This might be caused by a change in the underlying data source, a dependent analyst table, or a view."

    assert_has(:xpath, "//*[text()='#{expected_error}']")
  end

  defp create_new_selectable(type, name, query, opts \\ []) do
    visit_data_source(IntegrationTest.Manager.data_source_name())
    click({:xpath, "//a[text()='#{button_text(type)}']"})

    fill_field(name_input(type), name)

    click({:css, "#viewEditor"})
    send_text(query)

    if Keyword.get(opts, :submit_via_keyboard?),
      do: send_keys([:control, :enter]),
      else: click({:xpath, "//button[text()='Create']"})

    if Keyword.get(opts, :wait_until_created?, true) do
      wait_until_created(name)
      refresh_page()
    end
  end

  defp wait_until_created(name) do
    fn ->
      refresh_page()

      with false <- exists_visible?(:xpath, "#{selectable_row_xpath(name)}/..//a[text()='Edit']") do
        Process.sleep(1000)
        false
      end
    end
    |> Stream.repeatedly()
    |> Stream.take(10)
    |> Enum.find(&(&1 == true))
  end

  defp edit_selectable(type, name, changes) do
    visit_data_source(IntegrationTest.Manager.data_source_name())

    assert_has(nil, :xpath, "#{selectable_row_xpath(name)}/..//a[text()='Edit']", attempts: 50)
    click({:xpath, "#{selectable_row_xpath(name)}/..//a[text()='Edit']"})

    with {:ok, new_name} <- Keyword.fetch(changes, :name), do: fill_field(name_input(type), new_name)

    with {:ok, new_query} <- Keyword.fetch(changes, :query) do
      clear_code_mirror({:css, "#viewEditor"})
      send_text(new_query)
    end

    click({:xpath, "//button[text()='Update']"})
  end

  def selectable_row_xpath(name), do: "//div[./div/strong[text()='Tables and views']]//*[text()='#{name}']"

  defp button_text(:view), do: "New view"
  defp button_text(:table), do: "New table"

  defp name_input(:view), do: {:css, "#view_name"}
  defp name_input(:table), do: {:css, "#analyst_table_name"}
end
