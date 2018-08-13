defmodule Air.Service.LDAP.Test do
  # This test depends on the contents of ldap/bootstrap.ldif which is treated as a fixture

  use ExUnit.Case, async: true
  require Aircloak.DeployConfig

  alias Air.Service.LDAP
  alias Air.Service.LDAP.{User, Group}

  @regular_port 389
  @ssl_port 636
  @invalid_port 500
  @admin "cn=admin,dc=example,dc=org"
  @admin_pass "admin"

  describe "connecting" do
    test "without LDAP configured" do
      assert {:error, :ldap_not_configured} = LDAP.simple_bind(_no_config = :error, "user", "pass")
    end

    test "with wrong host/port" do
      assert {:error, :connect_failed} = LDAP.simple_bind({:ok, Map.put(ldap(), "port", @invalid_port)}, "user", "pass")
    end

    test "with invalid config" do
      assert {:error, :invalid_config} = LDAP.simple_bind({:ok, %{"some" => "stuff"}}, "user", "pass")
    end

    test "without SSL" do
      assert {:error, :invalid_credentials} = LDAP.simple_bind({:ok, ldap()}, "user", "pass")
    end

    test "with regular SSL" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind(
                 {:ok, Map.merge(ldap(), %{"encryption" => "ssl", "port" => @ssl_port})},
                 "user",
                 "pass"
               )
    end

    test "with StartTLS" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind({:ok, Map.put(ldap(), "encryption", "start_tls")}, "user", "pass")
    end
  end

  describe ".simple_bind" do
    test "with correct credentials" do
      assert :ok = LDAP.simple_bind({:ok, ldap()}, @admin, @admin_pass)
    end

    test "with incorrect credentials" do
      assert {:error, :invalid_credentials} = LDAP.simple_bind({:ok, ldap()}, "user", "pass")
    end

    test "with anonymous access" do
      assert :ok = LDAP.simple_bind({:ok, ldap()}, "", "")
    end
  end

  describe ".users" do
    test "finds all objects with valid login by default" do
      {:ok, entries} = LDAP.users({:ok, ldap()})

      assert [
               %User{login: "alice", dn: "cn=alice,ou=users,dc=example,dc=org", name: "alice"},
               %User{login: "bob", dn: "cn=bob,ou=users,dc=example,dc=org", name: "bob"}
             ] = Enum.sort(entries)
    end

    test "extracts the name and login fields as configured" do
      {:ok, entries} =
        LDAP.users({:ok, Map.merge(ldap(), %{"user_name" => "description", "user_login" => "description"})})

      assert [
               %User{
                 login: "An Alice",
                 dn: "cn=alice,ou=users,dc=example,dc=org",
                 name: "An Alice"
               }
             ] = Enum.sort(entries)
    end

    test "extracts users by filter" do
      assert {:ok, [%User{login: "alice"}]} =
               LDAP.users({:ok, Map.merge(ldap(), %{"user_filter" => "(description=An Alice)"})})
    end

    test "with an invalid filter" do
      assert {:error, :user_filter_invalid} = LDAP.users({:ok, Map.merge(ldap(), %{"user_filter" => "invalid"})})
    end

    test "extracts group list stored as user attributes" do
      assert {:ok,
              [
                %User{login: "alice", group_dns: ["An Alice"]},
                %User{login: "bob", group_dns: []}
              ]} = LDAP.users({:ok, Map.merge(ldap(), %{"user_group" => "description"})})
    end
  end

  describe ".groups" do
    test "finds all object by default" do
      {:ok, entries} = LDAP.groups({:ok, ldap()})

      assert [
               %Group{
                 dn: dn1 = "cn=group1,ou=groups,dc=example,dc=org",
                 name: dn1,
                 member_ids: ["alice", "bob"]
               },
               %Group{
                 dn: dn2 = "cn=group2,ou=groups,dc=example,dc=org",
                 name: dn2,
                 member_ids: ["alice"]
               },
               %Group{dn: dn3 = "ou=groups,dc=example,dc=org", name: dn3, member_ids: []}
             ] = Enum.sort(entries)
    end

    test "extracts the configured name" do
      {:ok, entries} = LDAP.groups({:ok, Map.put(ldap(), "group_name", "cn")})
      assert [%Group{name: "group1"}, %Group{name: "group2"}] = Enum.sort(entries)
    end

    test "extracts the configured member ids" do
      {:ok, entries} = LDAP.groups({:ok, Map.merge(ldap(), %{"group_member" => "description", "group_name" => "cn"})})

      assert [%Group{member_ids: ["A big group"]}, %Group{member_ids: ["A small group"]}] = Enum.sort(entries)
    end

    test "extracts groups by filter" do
      assert {:ok, [%Group{name: "cn=group1,ou=groups,dc=example,dc=org"}]} =
               LDAP.groups({:ok, Map.merge(ldap(), %{"group_filter" => "(description=A big group)"})})
    end

    test "with an invalid filter" do
      assert {:error, :group_filter_invalid} = LDAP.groups({:ok, Map.merge(ldap(), %{"group_filter" => "invalid"})})
    end
  end

  defp ldap() do
    {:ok, %{"host" => host}} = Aircloak.DeployConfig.fetch("ldap")

    %{
      "host" => host,
      "port" => @regular_port,
      "bind_dn" => @admin,
      "password" => @admin_pass,
      "user_base" => "ou=users,dc=example,dc=org",
      "group_base" => "ou=groups,dc=example,dc=org"
    }
  end
end
