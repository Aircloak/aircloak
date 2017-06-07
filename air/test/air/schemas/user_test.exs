defmodule Air.Schemas.UserTest do
  use Air.SchemaCase, async: true

  alias Air.{Schemas.Group, Schemas.User}

  test "role expansion" do
    assert [:user, :anonymous] == User.roles(user(:user))
    assert [:admin, :user, :anonymous] == User.roles(user(:admin))
  end

  test "permissions" do
    permissions = %{
      user: [:user_op],
      admin: [:admin_op]
    }

    assert true == User.permitted?(user(:user), :user_op, permissions)
    assert false == User.permitted?(user(:user), :admin_op, permissions)

    assert true == User.permitted?(user(:admin), :user_op, permissions)
    assert true == User.permitted?(user(:admin), :admin_op, permissions)
  end

  test "all permissions" do
    assert true == User.permitted?(user(:admin), :foo, %{admin: :all})
    assert true == User.permitted?(user(:admin), :bar, %{admin: :all})
    assert false == User.permitted?(user(:user), :foo, %{admin: :all})
  end

  test "anonymous permissions" do
    assert false == User.permitted?(nil, :foo, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(nil, :anon_op, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(user(:user), :anon_op, %{anonymous: [:anon_op], user: :all})
    assert true == User.permitted?(user(:admin), :anon_op, %{anonymous: [:anon_op], user: :all})
  end

  defp user(role_key) do
    %User{
      groups: [%Group{admin: role_key == :admin}]
    }
  end
end
