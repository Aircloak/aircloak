defmodule IntegrationTest.Acceptance.DataSourcesTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Manager

  test "shows the list of data sources" do
    login_as_admin()
    visit_admin_page("Data sources")
    assert_has(:xpath, "//td[text()='#{data_source_name()}']/..//*[contains(text(), 'Online')]")
  end

  test "showing a data source" do
    login_as_admin()
    visit_admin_page("Data sources")

    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Show']"})

    assert_has(:xpath, xpath_in_section("Users with access", "//*[text()='admin@aircloak.com']"))
    assert_has(:xpath, xpath_in_section("Groups granting access", "//*[text()='admins']"))
    assert_has(:xpath, xpath_in_section("Cloaks hosting data source", "//*[text()='#{Cloak.AirSocket.cloak_name()}']"))
    assert_has(:xpath, xpath_in_section("System tables", "//td[text()='integers']"))
    assert_has(:xpath, xpath_in_section("System tables", "//td[text()='users']"))
  end

  test "users with access edit link" do
    login_as_admin()
    visit_admin_page("Data sources")
    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Show']"})
    click({:xpath, xpath_in_section("Users with access", "//a[text()='Edit']")})
    assert current_path() == "/admin/data_sources/#{data_source_name()}/edit"
  end

  test "users with access edit user link" do
    login_as_admin()
    visit_admin_page("Data sources")

    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Show']"})
    click({:xpath, xpath_in_section("Users with access", "//*[text()='admin@aircloak.com']/..//a[text()='Edit user']")})
    admin_id = Air.Repo.get_by!(Air.Schemas.Login, login: "admin@aircloak.com").user_id
    assert current_path() == "/admin/users/#{admin_id}/edit"
  end

  test "groups granting access edit link" do
    login_as_admin()
    visit_admin_page("Data sources")

    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Show']"})
    click({:xpath, xpath_in_section("Groups granting access", "//a[text()='Edit']")})
    assert current_path() == "/admin/data_sources/#{data_source_name()}/edit"
  end

  test "groups granting access edit user link" do
    login_as_admin()
    visit_admin_page("Data sources")

    group = IntegrationTest.Manager.admin_group()

    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Show']"})

    click(
      {:xpath, xpath_in_section("Groups granting access", "//*[text()='#{group.name}']/..//a[text()='Edit group']")}
    )

    assert current_path() == "/admin/groups/#{group.id}/edit"
  end

  test "editing data source description" do
    login_as_admin()
    visit_admin_page("Data sources")
    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Edit']"})
    fill_field({:css, "#data_source_description"}, "foobar")
    click({:xpath, "//button[text()='Save']"})
    assert_has(:xpath, "//td[text()='#{data_source_name()}']/..//*[contains(text(), 'foobar')]")
  end

  test "editing data source group" do
    group = Air.Service.User.create_group!(%{name: new_group_name(), admin: false})

    login_as_admin()
    visit_admin_page("Data sources")

    # add group permissions
    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Edit']"})
    click({:xpath, "//*[text()='#{group.name}']/../input[@type='checkbox']"})
    click({:xpath, "//button[text()='Save']"})
    assert_has(:xpath, "//td[text()='#{data_source_name()}']/..//*[contains(text(), '#{group.name}')]")

    # remove group permissions
    click({:xpath, "//td[text()='#{data_source_name()}']/..//a[text()='Edit']"})
    click({:xpath, "//*[text()='#{group.name}']/../input[@type='checkbox']"})
    click({:xpath, "//button[text()='Save']"})
    refute_has(:xpath, "//td[text()='#{data_source_name()}']/..//*[contains(text(), '#{group.name}')]")
  end

  defp xpath_in_section(name, relative_path), do: "//h1[contains(text(), '#{name}')]/../..#{relative_path}"
end
