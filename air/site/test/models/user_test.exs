defmodule Air.UserTest do
  use Air.ModelCase, async: true

  alias Air.{User, TestRepoHelper, TestUtils, Group}

  @valid_attrs %{
    email: "admin@aircloak.com",
    password: "1234",
    password_confirmation: "1234",
    name: "Admin",
    organisation_id: 1,
    role_id: 0
  }
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = User.changeset(%User{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = User.changeset(%User{}, @invalid_attrs)
    refute changeset.valid?
  end

  test "validates email address" do
    attributes = %{@valid_attrs | email: "invalid_email"}
    assert errors_on(%User{}, :email, attributes)
  end

  test "validates password and confirmation" do
    attributes = %{@valid_attrs | password_confirmation: "wrong password"}
    assert errors_on(%User{}, :password_confirmation, attributes)
  end

  test "requires name to be two or more characters" do
    attributes = %{@valid_attrs | name: ""}
    assert errors_on(%User{}, :name, attributes)
  end

  test "requires an organisation" do
    attributes = %{@valid_attrs | organisation_id: ""}
    assert errors_on(%User{}, :organisation_id, attributes)
  end

  test "requires the role id" do
    attributes = %{@valid_attrs | role_id: nil}
    assert errors_on(%User{}, :role_id, attributes)
  end

  test "requires the valid id" do
    attributes = %{@valid_attrs | role_id: 314_159}
    assert errors_on(%User{}, :role_id, attributes)
  end

  test "only update hashed password on password change" do
    initial_changeset = Map.merge(%User{}, @valid_attrs)
    has_change_fn = fn(attr) ->
      %Ecto.Changeset{changes: changes} = User.changeset(initial_changeset, attr)
      Map.has_key?(changes, :hashed_password)
    end
    # We are simulating the user not editing the password text input boxes
    without_password_change = Map.drop(@valid_attrs, [:password, :password_confirmation])
    refute has_change_fn.(without_password_change)
    changed_password = %{@valid_attrs | password: "abcd", password_confirmation: "abcd"}
    assert has_change_fn.(changed_password)
  end

  test "role expansion" do
    assert [:user, :anonymous] == User.roles(user(:user))
    assert [:org_admin, :user, :anonymous] == User.roles(user(:org_admin))
    assert [:admin, :org_admin, :user, :anonymous] == User.roles(user(:admin))
  end

  test "permissions" do
    permissions = %{
      user: [:user_op],
      org_admin: [:org_admin_op],
      admin: [:admin_op]
    }

    assert true == User.permitted?(user(:user), :user_op, permissions)
    assert false == User.permitted?(user(:user), :org_admin_op, permissions)
    assert false == User.permitted?(user(:user), :admin_op, permissions)

    assert true == User.permitted?(user(:org_admin), :user_op, permissions)
    assert true == User.permitted?(user(:org_admin), :org_admin_op, permissions)
    assert false == User.permitted?(user(:org_admin), :admin_op, permissions)

    assert true == User.permitted?(user(:admin), :user_op, permissions)
    assert true == User.permitted?(user(:admin), :org_admin_op, permissions)
    assert true == User.permitted?(user(:admin), :admin_op, permissions)
  end

  test "correct verification of non-listed permissions" do
    assert false == User.permitted?(user(:user), :user_op, %{org_admin: [:user_op]})
    assert true == User.permitted?(user(:admin), :user_op, %{org_admin: [:user_op]})
  end

  test "all permissions" do
    assert true == User.permitted?(user(:admin), :foo, %{admin: :all})
    assert true == User.permitted?(user(:admin), :bar, %{admin: :all})
    assert false == User.permitted?(user(:org_admin), :foo, %{admin: :all})
    assert false == User.permitted?(user(:user), :foo, %{admin: :all})
  end

  test "anonymous permissions" do
    assert false == User.permitted?(nil, :foo, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(nil, :anon_op, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(user(:user), :anon_op, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(user(:org_admin), :anon_op, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(user(:admin), :anon_op, %{anonymous: [:anon_op], user: :all})
  end

  test "a user can have many groups" do
    org = TestRepoHelper.create_organisation!()
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(org, :user)
    |> set_groups([group1, group2])
    assert [group1.id, group2.id] == Enum.map(user.groups, &(&1.id)) |> Enum.sort()
  end

  test "deleting a user, doesn't delete the group" do
    org = TestRepoHelper.create_organisation!()
    group = TestRepoHelper.create_group!()
    TestRepoHelper.create_user!(org, :user)
    |> set_groups([group])
    |> Repo.delete()
    refute nil == Repo.get(Group, group.id)
  end

  test "deleting a user, should remove entries from join table" do
    org = TestRepoHelper.create_organisation!()
    group = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(org, :user)
    |> set_groups([group])
    TestUtils.assert_join_table_count_change(-1, fn() -> Repo.delete(user) end)
  end

  test "users and groups are joined through a join table" do
    org = TestRepoHelper.create_organisation!()
    group = TestRepoHelper.create_group!()
    TestUtils.assert_join_table_count_change(1, fn() ->
      TestRepoHelper.create_user!(org, :user)
      |> set_groups([group])
    end)
  end

  test "replacing a group for a user, removes the old relationship" do
    org = TestRepoHelper.create_organisation!()
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(org, :user)
    |> set_groups([group1])

    TestUtils.assert_join_table_count_change(0, fn() -> set_groups(user, [group2]) end)

    group1 = Repo.get(Group, group1.id) |> Repo.preload(:users)
    assert [] == group1.users
    group2 = Repo.get(Group, group2.id) |> Repo.preload(:users)
    assert [user.id] == Enum.map(group2.users, &(&1.id))
  end

  defp set_groups(user, groups) do
    Repo.get(User, user.id)
    |> Repo.preload(:groups)
    |> User.changeset()
    |> put_assoc(:groups, groups)
    |> Repo.update!()
    # Load the user from the database again, in case it will be used further,
    # just to ensure that the stored has the data we need, and that it isn't
    # just in a cached local version.
    Repo.get(User, user.id)
    |> Repo.preload(:groups)
  end

  defp user(role_key, org_name \\ ""),
    do: %User{
      role_id: User.role_id(role_key),
      organisation: %Air.Organisation{name: org_name},
      groups: [%Air.Group{admin: role_key == :admin}]
    }
end
