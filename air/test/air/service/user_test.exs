defmodule Air.Service.UserTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  import Aircloak.AssertionHelper

  alias Air.TestRepoHelper
  alias Air.Service.User

  describe "user operations" do
    test "required fields" do
      assert errors_on(&User.create/1, %{}) == [
               login: "can't be blank",
               name: "can't be blank"
             ]
    end

    test "requires non-empty login" do
      assert(error_on(&User.create/1, :login, "") == "can't be blank")
    end

    test "create cannot set the password" do
      User.create(%{
        login: "email@example.com",
        name: "Person",
        password: "password1234",
        password_confirmation: "password1234"
      })

      assert {:error, _} = User.login("email@example.com", "password1234")
    end

    test "create cannot set ldap_dn" do
      assert {:ok, %{ldap_dn: nil}} = User.create(%{login: "login", name: "Person", ldap_dn: "some dn"})
    end

    test "create_ldap can set ldap_dn" do
      assert {:ok, %{ldap_dn: "some dn"}} = User.create_ldap(%{login: "login", name: "Person", ldap_dn: "some dn"})
    end

    test "admin update cannot set the password" do
      user = TestRepoHelper.create_user!(%{password: "password1234"})
      User.update(user, %{password: "new password", password_confirmation: "new password"})
      assert {:error, _} = User.login("email@example.com", "new password")
    end

    test "update cannot change LDAP users" do
      assert_raise(RuntimeError, fn ->
        User.update(TestRepoHelper.create_user!(%{ldap_dn: "some dn"}), %{login: "new login"})
      end)
    end

    test "update with ldap: true cannot change non-LDAP users" do
      assert_raise(RuntimeError, fn ->
        User.update(TestRepoHelper.create_user!(%{ldap_dn: nil}), %{login: "new login"}, ldap: true)
      end)
    end

    test "update with ldap: true can change LDAP users" do
      assert {:ok, %{logins: [%{login: "new login"}]}} =
               User.update(TestRepoHelper.create_user!(%{ldap_dn: "some dn"}), %{login: "new login"}, ldap: true)
    end

    test "requires name to be two or more characters",
      do: assert(error_on(&User.create/1, :name, "a") == "should be at least 2 character(s)")

    test "only update hashed password on password change" do
      user = TestRepoHelper.create_user!(%{password: "password1234"})

      {:ok, updated_user} = User.update_full_profile(user, %{"name" => "foobar"})
      assert hd(updated_user.logins).hashed_password == hd(user.logins).hashed_password

      {:ok, updated_user} =
        User.update_full_profile(user, %{
          "old_password" => "password1234",
          "password" => "passwordwxyz",
          "password_confirmation" => "passwordwxyz"
        })

      refute hd(updated_user.logins).hashed_password == hd(user.logins).hashed_password
    end

    test "password is not changed if old password is invalid" do
      user = TestRepoHelper.create_user!(%{password: "password1234"})

      assert errors_on(
               &User.update_full_profile(user, &1),
               %{
                 "old_password" => "invalid",
                 "password" => "passwordwxyz",
                 "password_confirmation" => "passwordwxyz"
               }
             ) == [
               old_password: "Password invalid"
             ]

      assert {:error, :invalid_login_or_password} = User.login(hd(user.logins).login, "invalid")
    end

    test "the only admin can't be deleted",
      do: assert(User.delete(TestRepoHelper.create_only_user_as_admin!()) == {:error, :forbidden_no_active_admin})

    test "the only admin can't be updated to be a normal user",
      do:
        assert(
          User.update(TestRepoHelper.create_only_user_as_admin!(), %{groups: []}) ==
            {:error, :forbidden_no_active_admin}
        )

    test "a user can have many groups" do
      group1 = TestRepoHelper.create_group!()
      group2 = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group1.id, group2.id]})
      assert [group1.id, group2.id] == Enum.map(user.groups, & &1.id) |> Enum.sort()
    end

    test "replacing a group for a user, removes the old relationship" do
      group1 = TestRepoHelper.create_group!()
      group2 = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group1.id]})
      User.update!(user, %{groups: [group2.id]})
      user = User.load(user.id)
      assert [group2.id] == Enum.map(user.groups, & &1.id)
    end

    test "error in case of conflicting login" do
      user = TestRepoHelper.create_user!()

      assert errors_on(&User.create(&1), %{name: "Bob", login: User.main_login(user)})[:login] ==
               "has already been taken"
    end
  end

  describe ".create_app_login" do
    test "returns the login and password" do
      user = TestRepoHelper.create_user!()
      {:ok, login, password} = User.create_app_login(user, %{})

      user_id = user.id
      assert {:ok, %{id: ^user_id}} = User.login_psql(login, password)
    end

    test "the returned login and password can only be used in the psql scope" do
      {:ok, login, password} = User.create_app_login(TestRepoHelper.create_user!(), %{})
      assert {:error, :invalid_login_or_password} = User.login(login, password)
    end

    test "multiple calls create different logins with different passwords" do
      user = TestRepoHelper.create_user!()
      {:ok, login1, password1} = User.create_app_login(user, %{})
      {:ok, login2, password2} = User.create_app_login(user, %{})

      assert login1 != login2
      assert password1 != password2
    end
  end

  describe ".delete_app_login" do
    test "deletes the login" do
      user = TestRepoHelper.create_user!()
      {:ok, login, _} = User.create_app_login(user, %{})
      login_id = Repo.get_by(Air.Schemas.Login, login: login).id

      assert {:ok, _} = User.delete_app_login(user, login_id)
      assert is_nil(Repo.get(Air.Schemas.Login, login_id))
    end

    test "does not delete other user's logins" do
      user = TestRepoHelper.create_user!()
      {:ok, login, _} = User.create_app_login(TestRepoHelper.create_user!(), %{})
      login_id = Repo.get_by(Air.Schemas.Login, login: login).id

      assert :error = User.delete_app_login(user, login_id)
      refute is_nil(Repo.get(Air.Schemas.Login, login_id))
    end

    test "does not delete the main login" do
      user = TestRepoHelper.create_user!()
      login_id = hd(user.logins).id

      assert :error = User.delete_app_login(user, login_id)
      refute is_nil(Repo.get(Air.Schemas.Login, login_id))
    end
  end

  describe ".login" do
    test "success for native user" do
      user_id = TestRepoHelper.create_user!(%{login: "alice", password: "password1234"}).id
      assert {:ok, %{id: ^user_id}} = User.login("alice", "password1234")
    end

    test "failure for native user" do
      TestRepoHelper.create_user!(%{login: "alice", password: "password1234"})
      assert {:error, :invalid_login_or_password} = User.login("alice", "invalid password")
    end

    test "success for LDAP user" do
      user_id = TestRepoHelper.create_user!(%{login: "alice", ldap_dn: "cn=admin,dc=example,dc=org"}).id
      assert {:ok, %{id: ^user_id}} = User.login("alice", "admin")
    end

    test "failure for LDAP user" do
      TestRepoHelper.create_user!(%{login: "alice", ldap_dn: "cn=admin,dc=example,dc=org"})
      assert {:error, :invalid_login_or_password} = User.login("alice", "invalid_password")
    end

    test "disabled users cannot log in" do
      user = TestRepoHelper.create_user!(%{login: "alice", password: "password1234", enabled: false})
      User.disable(user)
      assert {:error, :invalid_login_or_password} = User.login("alice", "password1234")
    end
  end

  describe "deleting a user" do
    test "doesn't delete the group" do
      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      User.delete!(user)
      refute nil == User.load_group(group.id)
    end

    test "deletes all their queries" do
      user = TestRepoHelper.create_user!()

      query =
        TestRepoHelper.create_query!(user, %{
          data_source_id: TestRepoHelper.create_data_source!().id
        })

      TestRepoHelper.send_query_result(query.id, %{}, [%{occurrences: 1, row: ["some", "data"]}])

      User.delete!(user)

      assert is_nil(Repo.get(Air.Schemas.User, user.id))
      assert is_nil(Repo.get(Air.Schemas.Query, query.id))
    end

    test "deletes their views" do
      user = TestRepoHelper.create_user!()
      view = TestRepoHelper.create_view!(user, TestRepoHelper.create_data_source!())

      User.delete!(user)

      assert is_nil(Repo.get(Air.Schemas.View, view.id))
    end

    test "deletes their api_tokens" do
      user = TestRepoHelper.create_user!()
      token = TestRepoHelper.create_token!(user)

      User.delete!(user)

      assert is_nil(Repo.get(Air.Schemas.ApiToken, token.id))
    end

    test "deletes their audit logs" do
      user = TestRepoHelper.create_user!()
      :ok = Air.Service.AuditLog.log(user, "user delete test event", %{some: "metadata"})

      User.delete!(user)

      assert is_nil(Repo.get_by(Air.Schemas.AuditLog, event: "user delete test event"))
    end

    test "it's impossible to delete an enabled LDAP user" do
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})

      assert {:error, :invalid_ldap_delete} = User.delete(user)
      assert Repo.get(Air.Schemas.User, user.id)
    end

    test "it's possible to delete a disabled LDAP user" do
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})
      {:ok, user} = User.disable(user, ldap: true)

      assert {:ok, _} = User.delete(user)
      refute Repo.get(Air.Schemas.User, user.id)
    end
  end

  describe ".delete_async" do
    test "can delete a native user" do
      user = TestRepoHelper.create_user!()

      assert :ok = delete_async(user)
      assert soon(!Repo.get(Air.Schemas.User, user.id))
    end

    test "cannot delete an enabled ldap user" do
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})

      assert {:error, :invalid_ldap_delete} = delete_async(user)
      refute soon(!Repo.get(Air.Schemas.User, user.id))
    end

    test "can delete a disabled ldap user" do
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})
      {:ok, user} = User.disable(user, ldap: true)

      assert :ok = delete_async(user)
      assert soon(!Repo.get(Air.Schemas.User, user.id))
    end

    defp delete_async(user) do
      User.delete_async(user, fn -> nil end, fn -> nil end, fn _ -> nil end)
    end
  end

  describe "group operations" do
    test "required fields",
      do:
        assert(
          errors_on(&User.create_group/1, %{}) == [
            admin: "can't be blank",
            name: "can't be blank"
          ]
        )

    test "validates uniqueness of name" do
      User.create_group!(%{name: "group name", admin: false})

      assert errors_on(&User.create_group/1, %{name: "group name", admin: false})[:name] == "has already been taken"
    end

    test "connecting a group to users" do
      group = TestRepoHelper.create_group!()
      user1 = TestRepoHelper.create_user!(%{groups: [group.id]})
      user2 = TestRepoHelper.create_user!(%{groups: [group.id]})
      group = User.load_group(group.id)
      assert [user1.id, user2.id] == Enum.map(group.users, & &1.id) |> Enum.sort()
    end

    test "cannot assign LDAP users to a native group" do
      group = TestRepoHelper.create_group!() |> Repo.preload(:users)
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})

      assert errors_on(&User.create_group/1, %{admin: false, name: "group1", users: [user.id]})[:users] ==
               "cannot assign LDAP users to a native group"

      assert errors_on(&User.update_group(group, &1), %{users: [user.id]})[:users] ==
               "cannot assign LDAP users to a native group"
    end

    test "cannot assign native users to an LDAP group" do
      group = TestRepoHelper.create_group!(%{ldap_dn: "some dn"}) |> Repo.preload(:users)
      user = TestRepoHelper.create_user!()

      assert errors_on(&User.create_ldap_group/1, %{admin: false, name: "group1", users: [user.id]})[:users] ==
               "cannot assign native users to an LDAP group"

      assert errors_on(&User.update_group(group, &1, ldap: true), %{users: [user.id]})[:users] ==
               "cannot assign native users to an LDAP group"
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

      assert User.delete_group(non_deletable_admin_group) == {:error, :forbidden_no_active_admin}

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

      assert User.update_group(non_deletable_admin_group, %{admin: false}) == {:error, :forbidden_no_active_admin}

      assert [u1, u2] =
               User.load_group(non_deletable_admin_group.id).users
               |> Enum.sort_by(& &1.id)

      assert u1.id == admin1.id
      assert u2.id == admin2.id
    end
  end

  describe "toggle_debug_mode" do
    test "toggling changes state" do
      user = TestRepoHelper.create_user!()
      refute user.debug_mode_enabled == User.toggle_debug_mode(user).debug_mode_enabled
    end

    test "persists changes to db" do
      user = TestRepoHelper.create_user!()
      User.toggle_debug_mode(user)
      loaded_user = User.load(user.id)
      refute loaded_user.debug_mode_enabled == user.debug_mode_enabled
    end
  end

  describe "pseudonym" do
    # credo:disable-for-lines:2
    test "if no user is provided, a random pseudonym is generated",
      do: refute(User.pseudonym(nil) == User.pseudonym(nil))

    test "a users pseudonym does not change over time" do
      user_initial = TestRepoHelper.create_user!()
      pseudonym1 = User.pseudonym(user_initial)

      user_loaded = User.load(user_initial.id)
      pseudonym2 = User.pseudonym(user_loaded)

      assert pseudonym1 == pseudonym2
    end

    test "a stale user record does still get the same pseudonym" do
      user = TestRepoHelper.create_user!()
      pseudonym1 = User.pseudonym(user)
      pseudonym2 = User.pseudonym(user)

      assert pseudonym1 == pseudonym2
    end
  end

  describe ".logins" do
    test "returns main login" do
      user = TestRepoHelper.create_user!()
      main_login = User.main_login(user)
      assert main_login in User.logins(user)
    end

    test "returns a full list of logins" do
      user = TestRepoHelper.create_user!()
      login1 = TestRepoHelper.create_login_for_user!(user)
      login2 = TestRepoHelper.create_login_for_user!(user)
      user = User.load(user.id)

      expected_logins = Enum.sort([login1.login, login2.login, User.main_login(user)])
      actual_logins = user |> User.logins() |> Enum.sort()

      assert actual_logins == expected_logins
    end
  end

  describe "password reset" do
    setup do
      user = TestRepoHelper.create_user!()
      token = User.reset_password_token(user)
      {:ok, user: user, token: token}
    end

    test "cannot generate reset token for LDAP users" do
      user = TestRepoHelper.create_user!(%{ldap_dn: "some dn"})
      assert_raise(RuntimeError, fn -> User.reset_password_token(user) end)
    end

    test "with an invalid token" do
      assert {:error, :invalid_token} = User.reset_password("invalid token", %{})
    end

    test "cannot change fields", %{user: user, token: token} do
      old_name = user.name

      assert {:ok, %{name: ^old_name}} =
               User.reset_password(token, %{
                 password: "password1234",
                 password_confirmation: "password1234",
                 name: "new name"
               })
    end

    test "successful change", %{user: user, token: token} do
      assert {:ok, _} = User.reset_password(token, %{password: "new password", password_confirmation: "new password"})
      assert {:ok, _} = User.login(User.main_login(user), "new password")
    end

    test "incorrect confirmation", %{user: user, token: token} do
      assert {:error, _} = User.reset_password(token, %{password: "new password", password_confirmation: "other"})
      assert {:error, _} = User.login(User.main_login(user), "new password")
    end
  end

  describe "disabling and enabling users" do
    test "enabled by default", do: assert(User.is_enabled?(TestRepoHelper.create_user!()))

    test "toggling enabled state" do
      {:ok, user} = TestRepoHelper.create_user!() |> User.disable()
      refute User.is_enabled?(user)
      assert User.is_enabled?(User.enable!(user))
    end

    test "can't disable the last admin user" do
      user = TestRepoHelper.create_only_user_as_admin!()
      assert {:error, :forbidden_no_active_admin} = User.disable(user)
    end
  end

  describe ".update_group_data_sources" do
    test "can change data source assignments" do
      group = TestRepoHelper.create_group!() |> Air.Repo.preload(:data_sources)
      data_source = TestRepoHelper.create_data_source!()

      User.update_group_data_sources(group, %{data_sources: [data_source.id]})

      assert [%{id: data_source_id}] = User.load_group(group.id).data_sources
      assert data_source_id == data_source.id
    end

    test "cannot change other attributes" do
      group = TestRepoHelper.create_group!()

      User.update_group_data_sources(group, %{name: "new name"})

      refute User.load_group(group.id).name == "new name"
    end
  end

  describe ".update_profile_settings" do
    test "can change number settings" do
      user = TestRepoHelper.create_user!()

      assert {:ok, %{decimal_sep: ":", thousand_sep: "-", decimal_digits: 7}} =
               User.update_profile_settings(user, %{decimal_sep: ":", thousand_sep: "-", decimal_digits: 7})
    end

    test "cannot change login, name, and password" do
      user = TestRepoHelper.create_user!(%{login: "alice", name: "Alice"})

      assert {:ok, %{logins: [%{login: "alice"}], name: "Alice"}} =
               User.update_profile_settings(user, %{
                 login: "bob",
                 name: "Bob",
                 password: "new password",
                 password_confirmation: "new password"
               })

      assert {:error, _} = User.login("alice", "new password")
    end
  end

  describe ".to_changeset" do
    test "includes the main login" do
      user = TestRepoHelper.create_user!()
      changeset = User.to_changeset(user)

      Phoenix.HTML.Form.form_for(changeset, "/some/path", fn form ->
        assert Phoenix.HTML.Form.input_value(form, :login) == User.main_login(user)
        ""
      end)
    end
  end

  describe ".get_by_login" do
    test "returns not found for bogus login" do
      assert {:error, :not_found} = User.get_by_login("bogus")
    end

    test "returns user if exists" do
      user = TestRepoHelper.create_user!()
      assert {:ok, _} = user |> User.main_login() |> User.get_by_login()
    end
  end

  describe ".get_group_by_name" do
    test "returns not found for bogus name" do
      assert {:error, :not_found} = User.get_group_by_name("bogus")
    end

    test "returns group if exists" do
      group = TestRepoHelper.create_group!()
      assert {:ok, _} = User.get_group_by_name(group.name)
    end
  end

  describe ".add_preconfigured_user" do
    test "adds unknown user" do
      data = %{
        login: "login",
        password_hash: "hash"
      }

      assert {:ok, user} = User.add_preconfigured_user(data)
      assert [%{login: "login", hashed_password: "hash"}] = user.logins
    end

    test "ignores existing users" do
      user = TestRepoHelper.create_user!()

      data = %{
        login: User.main_login(user),
        password_hash: hd(user.logins).hashed_password
      }

      assert :error == User.add_preconfigured_user(data)
    end

    test "creates user accounts that can be used to log in" do
      data = %{
        login: "login",
        password_hash: Air.Service.Password.hash("password1234")
      }

      assert {:ok, user} = User.add_preconfigured_user(data)
      assert {:ok, _user} = User.login("login", "password1234")
    end

    test "creates admin users" do
      data = %{
        login: "login",
        password_hash: "hash",
        admin: true
      }

      assert {:ok, user} = User.add_preconfigured_user(data)
      assert user |> Air.Repo.preload(:groups) |> Air.Schemas.User.admin?()
    end
  end

  defp error_on(fun, field, value), do: errors_on(fun, %{field => value})[field]

  defp errors_on(fun, changes) do
    assert {:error, changeset} = fun.(changes)

    changeset
    |> Ecto.Changeset.traverse_errors(&AirWeb.ErrorHelpers.translate_error/1)
    |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
  end
end
