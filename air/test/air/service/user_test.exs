defmodule Air.Service.UserTest do
  use Air.SchemaCase, async: false # because of shared mode

  alias Air.{Service.User, TestRepoHelper}

  describe "user operations" do
    test "required fields", do:
      assert errors_on(&User.create/1, %{}) == [
        email: "can't be blank", name: "can't be blank", password: "can't be blank",
        password_confirmation: "can't be blank"
      ]

    test "validates email address", do:
      assert error_on(&User.create/1, :email, "invalid_email") == "has invalid format"

    test "validates password and confirmation", do:
      assert error_on(&User.create/1, :password_confirmation, "wrong password") == "does not match confirmation"

    test "requires name to be two or more characters", do:
      assert error_on(&User.create/1, :name, "a") == "should be at least 2 character(s)"

    test "requires password to be 4 or more characters", do:
      assert error_on(&User.create/1, :password, "123") == "should be at least 4 character(s)"

    test "only update hashed password on password change" do
      user = TestRepoHelper.create_user!()

      {:ok, updated_user} = User.update_profile(user, %{"name" => "foobar"})
      assert updated_user.hashed_password == user.hashed_password

      {:ok, updated_user} = User.update_profile(user,
        %{"old_password" => "1234", "password" => "wxyz", "password_confirmation" => "wxyz"})
      refute updated_user.hashed_password == user.hashed_password
    end

    test "the only admin can't be deleted", do:
      assert User.delete(TestRepoHelper.create_only_user_as_admin!()) == {:error, :forbidden_last_admin_deletion}

    test "the only admin can't be updated to be a normal user", do:
      assert User.update(TestRepoHelper.create_only_user_as_admin!(), %{groups: []}) ==
        {:error, :forbidden_last_admin_deletion}

    test "a user can have many groups" do
      group1 = TestRepoHelper.create_group!()
      group2 = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group1.id, group2.id]})
      assert [group1.id, group2.id] == Enum.map(user.groups, &(&1.id)) |> Enum.sort()
    end

    test "deleting a user, doesn't delete the group" do
      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      User.delete!(user)
      refute nil == User.load_group(group.id)
    end

    test "replacing a group for a user, removes the old relationship" do
      group1 = TestRepoHelper.create_group!()
      group2 = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group1.id]})
      User.update!(user, %{groups: [group2.id]})
      user = User.load(user.id)
      assert [group2.id] == Enum.map(user.groups, &(&1.id))
    end

    test "deleting a user deletes all their queries" do
      user = TestRepoHelper.create_user!()
      query = TestRepoHelper.create_query!(user, %{data_source_id: TestRepoHelper.create_data_source!().id})
      TestRepoHelper.send_query_result(query.id, %{}, [%{occurrences: 1, row: ["some", "data"]}])

      User.delete!(user)

      assert is_nil(Repo.get(Air.Schemas.User, user.id))
      assert is_nil(Repo.get(Air.Schemas.Query, query.id))
    end

    test "deleting a user deletes their views" do
      user = TestRepoHelper.create_user!()
      view = TestRepoHelper.create_view!(user, TestRepoHelper.create_data_source!())

      User.delete!(user)

      assert is_nil(Repo.get(Air.Schemas.View, view.id))
    end
  end

  describe "group operations" do
    test "required fields", do:
      assert errors_on(&User.create_group/1, %{}) == [admin: "can't be blank", name: "can't be blank"]

    test "validates uniqueness of name" do
      User.create_group!(%{name: "group name", admin: false})
      assert errors_on(&User.create_group/1, %{name: "group name", admin: false})[:name] == "has already been taken"
    end

    test "connecting a group to users" do
      group = TestRepoHelper.create_group!()
      user1 = TestRepoHelper.create_user!(%{groups: [group.id]})
      user2 = TestRepoHelper.create_user!(%{groups: [group.id]})
      group = User.load_group(group.id)
      assert [user1.id, user2.id] == Enum.map(group.users, &(&1.id)) |> Enum.sort()
    end

    test "connecting a group to a data source" do
      data_source = TestRepoHelper.create_data_source!()
      group = TestRepoHelper.create_group!(%{data_sources: [data_source.id]})
      group = User.load_group(group.id)
      assert [loaded_data_source] = group.data_sources
      assert loaded_data_source.id == data_source.id
    end

    test "deleting a group doesn't delete users or data sources" do
      data_source = TestRepoHelper.create_data_source!()
      group = TestRepoHelper.create_group!(%{data_sources: [data_source.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      User.delete_group!(group)
      refute nil == User.load(user.id)
      refute nil == Air.Repo.get(Air.Schemas.DataSource, data_source.id)
    end

    test "deleting the group is not allowed if it leads to no administrators" do
      admin1 = TestRepoHelper.create_only_user_as_admin!()
      deletable_admin_group = User.create_group!(%{name: "group name", admin: true})
      non_deletable_admin_group = hd(admin1.groups)
      admin2 = TestRepoHelper.create_user!(%{groups: [non_deletable_admin_group.id]})

      assert {:ok, _} = User.delete_group(deletable_admin_group)
      assert User.delete_group(non_deletable_admin_group) == {:error, :forbidden_last_admin_deletion}
      assert [u1, u2] = User.load_group(non_deletable_admin_group.id).users
      assert u1.id == admin1.id
      assert u2.id == admin2.id
    end

    test "unsetting the group admin flag is not allowed if it leads to no administrators" do
      admin1 = TestRepoHelper.create_only_user_as_admin!()
      deletable_admin_group = User.create_group!(%{name: "group name", admin: true})
      non_deletable_admin_group = hd(admin1.groups)
      admin2 = TestRepoHelper.create_user!(%{groups: [non_deletable_admin_group.id]})

      assert {:ok, _} = User.update_group(deletable_admin_group, %{admin: false})
      assert User.update_group(non_deletable_admin_group, %{admin: false}) == {:error, :forbidden_last_admin_deletion}
      assert [u1, u2] = User.load_group(non_deletable_admin_group.id).users
      assert u1.id == admin1.id
      assert u2.id == admin2.id
    end
  end

  defp error_on(fun, field, value), do:
    errors_on(fun, %{field => value})[field]

  defp errors_on(fun, changes) do
    assert {:error, changeset} = fun.(changes)

    changeset
    |> Ecto.Changeset.traverse_errors(&AirWeb.ErrorHelpers.translate_error/1)
    |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
  end
end
