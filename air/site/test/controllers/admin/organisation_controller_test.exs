defmodule Air.Admin.OrganisationControllerTest do
  use Air.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper

  test "users and admin_orgs have no permissions" do
    org = TestRepoHelper.create_organisation!()

    for role <- [:user, :org_admin] do
      user = TestRepoHelper.create_user!(org, role)

      assert "/" == login(user) |> get("/admin/organisations") |> redirected_to()
      assert "/" == login(user) |> get("/admin/organisations/new") |> redirected_to()
      assert "/" == login(user) |> get("/admin/organisations/#{user.id}/edit") |> redirected_to()
      assert "/" == login(user) |> post("/admin/organisations") |> redirected_to()
      assert "/" == login(user) |> put("/admin/organisations/#{user.id}") |> redirected_to()
      assert "/" == login(user) |> delete("/admin/organisations/#{user.id}") |> redirected_to()

      if role == :user do
        assert "/" == login(user) |> get("/admin/organisations/#{org.id}") |> redirected_to()
      end
    end
  end

  test "listing organisations" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)
    orgs = Enum.map(1..4, fn(_) -> TestRepoHelper.create_organisation!() end)

    orgs_html = login(admin) |> get("/admin/organisations") |> response(200)
    Enum.each([org | orgs], &assert(orgs_html =~ &1.name))
  end

  test "accessing new and edit forms" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)

    login(admin) |> get("/admin/organisations/new") |> response(200)
    login(admin) |> get("/admin/organisations/#{org.id}/edit") |> response(200)
  end

  test "viewing the organisation" do
    admin_org = TestRepoHelper.admin_organisation()
    another_org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(admin_org)
    user_from_another_org = TestRepoHelper.create_user!(another_org)

    html = login(admin) |> get("/admin/organisations/#{admin_org.id}") |> response(200)
    assert html =~ admin.email
    refute html =~ user_from_another_org.email
  end

  test "org admin can view its own organisation" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)
    another_org = TestRepoHelper.create_organisation!()

    login(org_admin) |> get("/admin/organisations/#{org.id}") |> response(200)
    assert "/" == login(org_admin) |> get("/admin/organisations/#{another_org.id}") |> redirected_to()
  end

  test "admin can view all organisations" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)
    another_org = TestRepoHelper.create_organisation!()

    login(admin) |> get("/admin/organisations/#{org.id}") |> response(200)
    login(admin) |> get("/admin/organisations/#{another_org.id}") |> response(200)
  end

  test "creating an organisation" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)

    new_org_name = "foobarbaz"

    conn =
    login(admin)
    |> post("/admin/organisations", organisation: %{name: new_org_name})

    assert "/admin/organisations" == redirected_to(conn)
    orgs_html = login(admin) |> get("/admin/organisations") |> response(200)
    assert orgs_html =~ new_org_name
  end

  test "updating an organisation" do
    org = TestRepoHelper.admin_organisation()
    another_org = TestRepoHelper.create_organisation!()
    admin = TestRepoHelper.create_user!(org)

    changed_name = "foobarbaz"

    conn =
    login(admin)
    |> put("/admin/organisations/#{another_org.id}", organisation: %{name: changed_name})

    assert "/admin/organisations/#{another_org.id}" == redirected_to(conn)
    orgs_html = login(admin) |> get("/admin/organisations") |> response(200)
    assert orgs_html =~ changed_name
    refute orgs_html =~ another_org.name
  end

  test "can't update admin organisation name" do
    error =
    TestRepoHelper.admin_organisation()
    |> Air.Organisation.changeset(%{name: "changed_name"})
    |> Air.Repo.update()
    |> catch_error()

    assert %Postgrex.Error{postgres: %{message: message}} = error
    assert "can't change administrators name" == message
    assert Air.Organisation.admin_group_name() == TestRepoHelper.admin_organisation().name
  end

  test "can't delete admin organisation" do
    error =
    TestRepoHelper.admin_organisation()
    |> Repo.delete!()
    |> catch_error()

    assert %Postgrex.Error{postgres: %{message: message}} = error
    assert "can't delete administrators group" == message
    assert Air.Organisation.admin_group_name() == TestRepoHelper.admin_organisation().name
  end

  test "deleting an organisation" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org)
    another_org = TestRepoHelper.create_organisation!()

    assert "/admin/organisations" == login(admin)
      |> delete("/admin/organisations/#{another_org.id}") |> redirected_to()
    orgs_html = login(admin) |> get("/admin/organisations") |> response(200)
    refute orgs_html =~ another_org.name
  end
end
