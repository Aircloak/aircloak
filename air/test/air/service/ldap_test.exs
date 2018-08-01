defmodule Air.Service.LDAP.Test do
  use ExUnit.Case, async: true

  alias Air.Service.LDAP
  alias Air.Service.LDAP.User

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
    "user_base" => "dc=example,dc=org"
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
    test "finds all objects by default" do
      {:ok, entries} = LDAP.users({:ok, @ldap})

      assert [
               %User{dn: @admin, name: @admin},
               %User{dn: "dc=example,dc=org", name: "dc=example,dc=org"}
             ] = Enum.sort(entries)
    end
  end
end
