defmodule Air.Service.LDAP.Sync.Test do
  use Air.SchemaCase

  import Air.TestRepoHelper

  alias Air.Service.LDAP.{Group, Sync, User}

  describe "syncing users" do
    test "creating a user from LDAP" do
      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])

      user = Air.Repo.get_by(Air.Schemas.User, ldap_dn: "some dn", name: "Alice") |> Air.Repo.preload(:logins)
      assert user
      assert Air.Service.User.main_login(user) == "alice"
    end

    test "LDAP user not created if such Aircloak user exists" do
      create_user!(%{login: "alice"})

      ExUnit.CaptureLog.capture_log(fn ->
        Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice"}], _groups = [])
      end)

      refute Air.Repo.get_by(Air.Schemas.User, ldap_dn: "some dn", name: "Alice")
    end

    test "user updated if already synced" do
      user = create_user!(%{login: "alice", name: "Alice", ldap_dn: "some dn"})

      Sync.sync([%User{dn: "some dn", login: "alice", name: "Alice the Magnificent"}], _groups = [])

      assert %{name: "Alice the Magnificent"} = Air.Repo.get(Air.Schemas.User, user.id)
    end

    test "user conflicts caused by a change in LDAP" do
      user_from_ldap = create_user!(%{login: "alice", name: "Alice", ldap_dn: "some dn"})
      create_user!(%{login: "bob", name: "Bob"})

      ExUnit.CaptureLog.capture_log(fn ->
        Sync.sync([%User{dn: "some dn", login: "bob", name: "Bob"}], _groups = [])
      end)

      refute Air.Repo.get(Air.Schemas.User, user_from_ldap.id).enabled
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

    test "group conflicts with a removed LDAP group" do
      create_group!(%{name: "group1", ldap_dn: "some dn"})

      Sync.sync(_users = [], [%Group{dn: "some other dn", name: "group1", member_ids: []}])

      refute Air.Repo.get_by(Air.Schemas.Group, ldap_dn: "some dn")
      assert Air.Repo.get_by(Air.Schemas.Group, name: "group1", ldap_dn: "some other dn")
    end

    test "two conflicting groups arrive from LDAP" do
      ExUnit.CaptureLog.capture_log(fn ->
        Sync.sync(_users = [], [
          %Group{dn: "some dn", name: "group1", member_ids: []},
          %Group{dn: "some other dn", name: "group1", member_ids: []}
        ])
      end)

      assert Air.Repo.get_by(Air.Schemas.Group, name: "group1", source: :ldap, ldap_dn: "some dn")
      refute Air.Repo.get_by(Air.Schemas.Group, ldap_dn: "some other dn")
    end

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
               Air.Repo.get_by(Air.Schemas.User, name: "Alice") |> Air.Repo.preload(:groups)

      assert %{groups: [%{name: "group1"}]} =
               Air.Repo.get_by(Air.Schemas.User, name: "Bob") |> Air.Repo.preload(:groups)
    end

    test "updating group members" do
      group = create_group!(%{name: "group1", ldap_dn: "group dn 1"})

      Sync.sync(
        [%User{dn: "some dn", login: "alice", name: "Alice"}, %User{dn: "other dn", login: "bob", name: "Bob"}],
        [%Group{dn: "group dn 1", name: "group1", member_ids: ["alice", "bob"]}]
      )

      assert %{users: [%{name: "Alice"}, %{name: "Bob"}]} =
               Air.Repo.get(Air.Schemas.Group, group.id) |> Air.Repo.preload(:users)
    end
  end

  test ".sync is idempotent" do
    users = [%User{dn: "some dn", login: "alice", name: "Alice"}, %User{dn: "other dn", login: "bob", name: "Bob"}]

    groups = [
      %Group{dn: "group dn 1", name: "group1", member_ids: ["alice", "bob"]},
      %Group{dn: "group dn 2", name: "group2", member_ids: ["alice"]}
    ]

    Sync.sync(users, groups)
    intermediate_results = {Air.Repo.all(Air.Schemas.User), Air.Repo.preload(Air.Repo.all(Air.Schemas.Group), :users)}
    Sync.sync(users, groups)
    final_results = {Air.Repo.all(Air.Schemas.User), Air.Repo.preload(Air.Repo.all(Air.Schemas.Group), :users)}

    assert intermediate_results == final_results
  end
end
