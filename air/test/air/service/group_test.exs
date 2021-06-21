defmodule Air.Service.Group.Test do
  use Air.SchemaCase

  alias Air.Service.{User, Group}
  alias Air.TestRepoHelper

  describe "basic operations" do
    test "required fields" do
      assert errors_on(&Group.create/1, %{}) == [
               admin: "can't be blank",
               name: "can't be blank"
             ]
    end

    test "validates uniqueness of name" do
      Group.create!(%{name: "group name", admin: false})

      assert errors_on(&Group.create/1, %{name: "group name", admin: false})[:name] == "has already been taken"
    end

    test "connecting a group to users" do
      group = TestRepoHelper.create_group!()
      user1 = TestRepoHelper.create_user!(%{groups: [group.id]})
      user2 = TestRepoHelper.create_user!(%{groups: [group.id]})
      group = Group.load(group.id)
      assert [user1.id, user2.id] == Enum.map(group.users, & &1.id) |> Enum.sort()
    end

    test "cannot assign LDAP users to a native group" do
      group = TestRepoHelper.create_group!() |> Repo.preload(:users)
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})

      assert errors_on(&Group.create/1, %{admin: false, name: "group1", users: [user.id]})[:users] ==
               "cannot assign LDAP users to a native group"

      assert errors_on(&Group.update(group, &1), %{users: [user.id]})[:users] ==
               "cannot assign LDAP users to a native group"
    end

    test "cannot assign native users to an LDAP group" do
      group = TestRepoHelper.create_group!(%{ldap_dn: "some dn"}) |> Repo.preload(:users)
      user = TestRepoHelper.create_user!()

      assert errors_on(&Group.create_ldap/1, %{admin: false, name: "group1", users: [user.id]})[:users] ==
               "cannot assign native users to an LDAP group"

      assert errors_on(&Group.update(group, &1, ldap: true), %{users: [user.id]})[:users] ==
               "cannot assign native users to an LDAP group"
    end

    test "connecting a group to a data source" do
      data_source = TestRepoHelper.create_data_source!()
      group = TestRepoHelper.create_group!(%{data_sources: [data_source.id]})
      group = Group.load(group.id)
      assert [loaded_data_source] = group.data_sources
      assert loaded_data_source.id == data_source.id
    end

    test "deleting a group doesn't delete users or data sources" do
      data_source = TestRepoHelper.create_data_source!()
      group = TestRepoHelper.create_group!(%{data_sources: [data_source.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      Group.delete!(group)
      assert {:ok, _} = User.load(user.id)
      refute nil == Air.Repo.get(Air.Schemas.DataSource, data_source.id)
    end

    test "deleting the group is not allowed if it leads to no administrators" do
      admin1 = TestRepoHelper.create_only_admin_user!()
      deletable_admin_group = Group.create!(%{name: "group name", admin: true})
      non_deletable_admin_group = hd(admin1.groups)
      admin2 = TestRepoHelper.create_user!(%{groups: [non_deletable_admin_group.id]})

      assert {:ok, _} = Group.delete(deletable_admin_group)

      assert Group.delete(non_deletable_admin_group) == {:error, :forbidden_no_active_admin}

      assert [u1, u2] = Group.load(non_deletable_admin_group.id).users
      assert u1.id == admin1.id
      assert u2.id == admin2.id
    end

    test "unsetting the group admin flag is not allowed if it leads to no administrators" do
      admin1 = TestRepoHelper.create_only_admin_user!()
      deletable_admin_group = Group.create!(%{name: "group name", admin: true})
      non_deletable_admin_group = hd(admin1.groups)
      admin2 = TestRepoHelper.create_user!(%{groups: [non_deletable_admin_group.id]})

      assert {:ok, _} = Group.update(deletable_admin_group, %{admin: false})

      assert Group.update(non_deletable_admin_group, %{admin: false}) == {:error, :forbidden_no_active_admin}

      assert [u1, u2] =
               Group.load(non_deletable_admin_group.id).users
               |> Enum.sort_by(& &1.id)

      assert u1.id == admin1.id
      assert u2.id == admin2.id
    end

    defp errors_on(fun, changes) do
      assert {:error, changeset} = fun.(changes)

      changeset
      |> Ecto.Changeset.traverse_errors(&AirWeb.ErrorHelpers.translate_error/1)
      |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
    end
  end

  describe ".update_data_sources" do
    test "can remove all data sources from group" do
      data_source = TestRepoHelper.create_data_source!()
      group = TestRepoHelper.create_group!(%{data_sources: [data_source.id]})
      assert [%{id: data_source_id}] = Group.load(group.id).data_sources
      assert data_source_id == data_source.id

      Group.update_data_sources(group, %{data_sources: []})

      assert [] == Group.load(group.id).data_sources
    end

    test "can change data source assignments" do
      group = TestRepoHelper.create_group!() |> Air.Repo.preload(:data_sources)
      data_source = TestRepoHelper.create_data_source!()

      Group.update_data_sources(group, %{data_sources: [data_source.id]})

      assert [%{id: data_source_id}] = Group.load(group.id).data_sources
      assert data_source_id == data_source.id
    end

    test "cannot change other attributes" do
      group = TestRepoHelper.create_group!()

      Group.update_data_sources(group, %{name: "new name"})

      refute Group.load(group.id).name == "new name"
    end
  end

  describe ".get_by_name" do
    test "returns not found for bogus name" do
      assert {:error, :not_found} = Group.get_by_name("bogus")
    end

    test "returns group if exists" do
      group = TestRepoHelper.create_group!()
      assert {:ok, _} = Group.get_by_name(group.name)
    end
  end

  describe ".available_to_user" do
    setup do
      native_group = TestRepoHelper.create_group!()
      ldap_group = TestRepoHelper.create_group!(%{ldap_dn: "DN"})

      TestRepoHelper.create_group!(%{system: true})
      TestRepoHelper.create_group!(%{ldap_dn: "DN-system", system: true})

      %{native_group: native_group, ldap_group: ldap_group}
    end

    test("a native user should only see non-system native groups",
      do:
        assert(
          TestRepoHelper.create_user!()
          |> Group.available_to_user()
          |> Enum.all?(&(&1.source == :native and not &1.system)),
          "Only non-system native groups"
        )
    )

    test("an ldap user should only see non-system ldap groups",
      do:
        assert(
          TestRepoHelper.create_user!(%{ldap_dn: "some dn"})
          |> Group.available_to_user()
          |> Enum.all?(&(&1.source == :ldap and not &1.system)),
          "Only non-system groups from LDAP"
        )
    )
  end

  describe ".all_native_user_groups" do
    setup do
      native_group = TestRepoHelper.create_group!()
      ldap_group = TestRepoHelper.create_group!(%{ldap_dn: "DN"})

      TestRepoHelper.create_group!(%{system: true})
      TestRepoHelper.create_group!(%{ldap_dn: "DN-system", system: true})

      %{native_group: native_group, ldap_group: ldap_group}
    end

    test("should only contain native non-system groups",
      do:
        assert(
          Group.all_native_user_groups()
          |> Enum.all?(&(&1.source == :native and not &1.system)),
          "Only non-system native groups"
        )
    )
  end
end
