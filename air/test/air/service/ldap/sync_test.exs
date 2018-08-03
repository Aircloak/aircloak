defmodule Air.Service.LDAP.Sync.Test do
  use Air.SchemaCase

  alias Air.Service.LDAP.{Sync, User}

  describe "syncing users" do
    test "creating a user from LDAP" do
      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])
      assert Air.Repo.get_by(Air.Schemas.User, ldap_dn: "some dn", login: "alice", name: "Alice")
    end

    @tag :pending
    test "LDAP user not created if such Aircloak user exists"

    @tag :pending
    test "user updated if already synced"

    @tag :pending
    test "user deactivated if no longer in LDAP"
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
