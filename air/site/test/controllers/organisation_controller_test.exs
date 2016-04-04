defmodule Air.OrganisationControllerTest do
  use Air.ConnCase, async: false

  alias Air.{TestConnHelper, TestRepoHelper}

  test "users and admin_orgs have no permissions" do
    org = TestRepoHelper.create_organisation!()

    for role <- [:user, :org_admin] do
      user = TestRepoHelper.create_user!(org, role)

      assert "/" == login(user) |> get("/organisations") |> redirected_to()
      assert "/" == login(user) |> get("/organisations/new") |> redirected_to()
      assert "/" == login(user) |> get("/organisations/#{user.id}/edit") |> redirected_to()
      assert "/" == login(user) |> post("/organisations") |> redirected_to()
      assert "/" == login(user) |> put("/organisations/#{user.id}") |> redirected_to()
      assert "/" == login(user) |> delete("/organisations/#{user.id}") |> redirected_to()
    end
  end

  test "listing organisations" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)
    orgs = Enum.map(1..4, fn(_) -> TestRepoHelper.create_organisation!() end)

    orgs_html = login(admin) |> get("/organisations") |> response(200)
    Enum.each([org | orgs], &assert(orgs_html =~ &1.name))
  end

  test "accessing new and edit forms" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)

    login(admin) |> get("/organisations/new") |> response(200)
    login(admin) |> get("/organisations/#{org.id}/edit") |> response(200)
  end

  test "viewing the organisation" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)

    html = login(admin) |> get("/organisations/#{org.id}") |> response(200)
    assert html =~ admin.email
  end

  test "creating an organisation" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)

    new_org_name = "foobarbaz"

    conn =
      login(admin)
      |> post("/organisations", organisation: %{name: new_org_name})

    assert "/organisations" == redirected_to(conn)
    orgs_html = login(admin) |> get("/organisations") |> response(200)
    assert orgs_html =~ new_org_name
  end

  test "updating an organisation" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)

    changed_name = "foobarbaz"

    conn =
      login(admin)
      |> put("/organisations/#{org.id}", organisation: %{name: changed_name})

    assert "/organisations/#{org.id}" == redirected_to(conn)
    orgs_html = login(admin) |> get("/organisations") |> response(200)
    assert orgs_html =~ changed_name
    refute orgs_html =~ org.name
  end

  test "deleting an organisation" do
    org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org, :admin)
    another_org = TestRepoHelper.create_organisation!()

    assert "/organisations" == login(admin) |> delete("/organisations/#{another_org.id}") |> redirected_to()
    orgs_html = login(admin) |> get("/organisations") |> response(200)
    refute orgs_html =~ another_org.name
  end

  defp login(user),
    do: TestConnHelper.login(conn(), user)
end
