defmodule Air.Service.LDAP.Normalization.Test do
  use ExUnit.Case, async: true

  alias Air.Service.LDAP.{Normalization, User, Group}

  test "moving membership information from users to groups" do
    users = [
      %User{login: "alice", name: "Alice", dn: "cn=Alice Liddel,dc=wonderland,dc=io", group_dns: ["group1", "group2"]},
      %User{login: "bob", name: "Bob", dn: "cn=Bob Ross,dc=you,dc=can,dc=paint,dc=com", group_dns: ["group1"]},
      %User{login: "charlie", name: "Charlie", dn: "cn=Charles Xavier,dc=x,dc=men,dc=org", group_dns: []}
    ]

    groups = [
      %Group{name: "group1", dn: "group1", member_ids: []},
      %Group{name: "group2", dn: "group2", member_ids: ["charlie"]},
      %Group{name: "group3", dn: "group3", member_ids: []}
    ]

    assert {^users,
            [
              %Group{name: "group1", dn: "group1", member_ids: ["alice", "bob"]},
              %Group{name: "group2", dn: "group2", member_ids: ["alice", "charlie"]},
              %Group{name: "group3", dn: "group3", member_ids: []}
            ]} = Normalization.normalize(users, groups)
  end
end
