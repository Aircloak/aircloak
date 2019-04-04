defmodule IntegrationTest.Acceptance.GroupTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "allows adding a group" do
    name = new_group_name()

    login_as_admin()
    |> add_group(name)
    |> assert_has(css("td", text: name))
  end

  test "allows removing a group" do
    name = new_group_name()

    login_as_admin()
    |> add_group(name)
    |> accept_confirm!(&click(&1, xpath("//tr[td[text()='#{name}']]//a[text()='Delete']")))
    |> refute_has(css("td", text: name))
  end

  test "forbids access to data source by default" do
    group = Air.Service.User.create_group!(%{name: new_group_name(), admin: false})
    user = Manager.create_air_user(group)

    session =
      login_as_admin()
      |> visit("/admin/data_sources")
      |> click(xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//a[text()='Show']"))

    refute_has(session, xpath(".//td[text()='#{user.name}']"))
    refute_has(session, xpath(".//td[text()='#{group.name}']"))
  end

  test "allowing access to a data source through a group" do
    group = Air.Service.User.create_group!(%{name: new_group_name(), admin: false})
    user = Manager.create_air_user(group)

    session =
      login_as_admin()
      |> visit("/admin/groups")
      |> click(xpath("//tr[td[text()='#{group.name}']]//a[text()='Edit']"))
      |> click(xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//input[@type='checkbox']"))
      |> click(xpath("//button[text()='Save group']"))
      |> visit("/admin/data_sources")
      |> click(xpath("//tr[.//*[text()='#{Manager.data_source_name()}']]//a[text()='Show']"))

    assert_has(session, xpath(".//td[text()='#{user.name}']"))
    assert_has(session, xpath(".//td[text()='#{group.name}']"))
  end
end
