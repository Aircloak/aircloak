defmodule Air.Service.LDAP.Sync.Test do
  use Air.SchemaCase

  import Air.TestRepoHelper

  alias Air.Service.LDAP.{Sync, User}

  describe "syncing users" do
    test "creating a user from LDAP" do
      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])
      assert Air.Repo.get_by(Air.Schemas.User, ldap_dn: "some dn", login: "alice", name: "Alice")
    end

    test "LDAP user not created if such Aircloak user exists" do
      create_user!(%{login: "alice"})

      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])

      refute Air.Repo.get_by(Air.Schemas.User, ldap_dn: "some dn", login: "alice", name: "Alice")
    end

    test "user updated if already synced" do
      user = create_user!(%{login: "alice", name: "Alice", ldap_dn: "some dn"})

      Sync.sync([%User{dn: "some dn", login: "bob", name: "Bob"}], _groups = [])

      assert %{login: "bob", name: "Bob"} = Air.Repo.get(Air.Schemas.User, user.id)
    end

    test "user deactivated if no longer in LDAP" do
      user = create_user!(%{ldap_dn: "some dn"})

      Sync.sync(_users = [], _groups = [])

      refute Air.Repo.get(Air.Schemas.User, user.id).enabled
    end

    test "user reactivated if they appear in LDAP again" do
      user = create_user!(%{ldap_dn: "some dn", enabled: false})

      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])

      assert Air.Repo.get(Air.Schemas.User, user.id).enabled
    end
  end

  describe "syncing groups" do
    @tag :pending
    test "creating a group"

    @tag :pending
    test "assinging group members"

    @tag :pending
    test "LDAP group not created if such Aircloak group exists"

    @tag :pending
    test "group updated if already synced"

    @tag :pending
    test "group deleted if no longer in LDAP"
  end
end
