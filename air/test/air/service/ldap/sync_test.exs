defmodule Air.Service.LDAP.Sync.Test do
  use Air.SchemaCase

  import Air.TestRepoHelper

  alias Air.Service.LDAP.{Sync, User, Group}

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
      user = create_user!(%{ldap_dn: "some dn"})
      Air.Service.User.disable(user, ldap: true)

      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])

      assert Air.Repo.get(Air.Schemas.User, user.id).enabled
    end
  end

  describe "syncing groups" do
    test "creating a group" do
      Sync.sync(_users = [], [%Group{dn: "some dn", name: "group1", member_ids: []}])
      assert Air.Repo.get_by(Air.Schemas.Group, ldap_dn: "some dn", name: "group1", source: :ldap)
    end

    test "LDAP groups and Aircloak-native groups have distinct namespaces" do
      create_group!(%{name: "group1"})

      Sync.sync(_users = [], [%Group{dn: "some dn", name: "group1", member_ids: []}])

      assert Air.Repo.get_by(Air.Schemas.Group, name: "group1", source: :native)
      assert Air.Repo.get_by(Air.Schemas.Group, name: "group1", source: :ldap, ldap_dn: "some dn")
    end

    test "group updated if already synced" do
      create_group!(%{name: "group1", ldap_dn: "some dn"})

      Sync.sync(_users = [], [%Group{dn: "some dn", name: "group2", member_ids: []}])

      assert Air.Repo.get_by(Air.Schemas.Group, name: "group2", source: :ldap, ldap_dn: "some dn")
    end

    @tag :pending
    test "two conflicting groups arrive from LDAP"

    test "group deleted if no longer in LDAP" do
      group = create_group!(%{name: "group1", ldap_dn: "some dn"})

      Sync.sync(_users = [], _groups = [])

      refute Air.Repo.get(Air.Schemas.Group, group.id)
    end

    test "assinging group members" do
      Sync.sync(
        [%User{dn: "some dn", login: "alice", name: "Alice"}, %User{dn: "other dn", login: "bob", name: "Bob"}],
        [
          %Group{dn: "group dn 1", name: "group1", member_ids: ["alice", "bob"]},
          %Group{dn: "group dn 2", name: "group2", member_ids: ["alice"]}
        ]
      )

      assert %{groups: [%{name: "group1"}, %{name: "group2"}]} =
               Air.Repo.get_by(Air.Schemas.User, login: "alice") |> Air.Repo.preload(:groups)

      assert %{groups: [%{name: "group1"}]} =
               Air.Repo.get_by(Air.Schemas.User, login: "bob") |> Air.Repo.preload(:groups)
    end

    @tag :pending
    test "updating group members"

    @tag :pending
    test "group members not changed for groups that didn't sync because they already exist"
  end
end
