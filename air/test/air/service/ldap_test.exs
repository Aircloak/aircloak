defmodule Air.Service.LDAP.Test do
  # This test depends on the contents of ldap/bootstrap.ldif which is treated as a fixture

  use ExUnit.Case, async: true

  alias Air.Service.LDAP
  alias Air.Service.LDAP.{User, Group}

  @regular_port 389
  @ssl_port 636
  @invalid_port 500
  @admin "cn=admin,dc=example,dc=org"
  @admin_pass "admin"

  @ldap %{
    "host" => "localhost",
    "port" => @regular_port,
    "bind_dn" => @admin,
    "password" => @admin_pass,
    "user_base" => "ou=users,dc=example,dc=org",
    "group_base" => "ou=groups,dc=example,dc=org"
  }

  describe "connecting" do
    test "without LDAP configured" do
      assert {:error, :ldap_not_configured} = LDAP.simple_bind(_no_config = :error, "user", "pass")
    end

    test "with wrong host/port" do
      assert {:error, :connect_failed} = LDAP.simple_bind({:ok, Map.put(@ldap, "port", @invalid_port)}, "user", "pass")
    end

    test "with invalid config" do
      assert {:error, :invalid_config} = LDAP.simple_bind({:ok, %{"some" => "stuff"}}, "user", "pass")
    end

    test "without SSL" do
      assert {:error, :invalid_credentials} = LDAP.simple_bind({:ok, @ldap}, "user", "pass")
    end

    test "with regular SSL" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind({:ok, Map.merge(@ldap, %{"encryption" => "ssl", "port" => @ssl_port})}, "user", "pass")
    end

    test "with StartTLS" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind({:ok, Map.put(@ldap, "encryption", "start_tls")}, "user", "pass")
    end
  end

  describe ".simple_bind" do
    test "with correct credentials" do
      assert :ok = LDAP.simple_bind({:ok, @ldap}, @admin, @admin_pass)
    end

    test "with incorrect credentials" do
      assert {:error, :invalid_credentials} = LDAP.simple_bind({:ok, @ldap}, "user", "pass")
    end

    test "with anonymous access" do
      assert :ok = LDAP.simple_bind({:ok, @ldap}, "", "")
    end
  end

  describe ".users" do
    test "finds all objects with valid login by default" do
      {:ok, entries} = LDAP.users({:ok, @ldap})

      assert [
               %User{login: "alice", dn: "cn=alice,ou=users,dc=example,dc=org", name: "alice"},
               %User{login: "bob", dn: "cn=bob,ou=users,dc=example,dc=org", name: "bob"}
             ] = Enum.sort(entries)
    end

    test "extracts the name and login fields as configured" do
      {:ok, entries} =
        LDAP.users({:ok, Map.merge(@ldap, %{"user_name" => "description", "user_login" => "description"})})

      assert [
               %User{login: "An Alice", dn: "cn=alice,ou=users,dc=example,dc=org", name: "An Alice"}
             ] = Enum.sort(entries)
    end
  end

  describe ".groups" do
    test "finds all object with valid name by default" do
      {:ok, entries} = LDAP.groups({:ok, @ldap})

      assert [
               %Group{dn: "cn=group1,ou=groups,dc=example,dc=org", name: "group1", member_ids: ["alice", "bob"]},
               %Group{dn: "cn=group2,ou=groups,dc=example,dc=org", name: "group2", member_ids: ["alice"]}
             ] = Enum.sort(entries)
    end

    test "extracts the configured name" do
      {:ok, entries} = LDAP.groups({:ok, Map.put(@ldap, "group_name", "description")})
      assert [%Group{name: "A big group"}, %Group{name: "A small group"}] = Enum.sort(entries)
    end

    test "extracts the configured member ids" do
      {:ok, entries} = LDAP.groups({:ok, Map.put(@ldap, "group_member", "description")})
      assert [%Group{member_ids: ["A big group"]}, %Group{member_ids: ["A small group"]}] = Enum.sort(entries)
    end
  end
end
