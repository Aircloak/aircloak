defmodule Air.Service.LDAP.Test do
  use ExUnit.Case, async: true

  alias Air.Service.LDAP

  describe "connecting" do
    test "without LDAP configured" do
      assert {:error, :ldap_not_configured} = LDAP.simple_bind(_no_config = :error, "user", "pass")
    end

    test "with wrong host/port" do
      assert {:error, :connect_failed} =
               LDAP.simple_bind({:ok, %{"host" => "localhost", "port" => 500}}, "user", "pass")
    end

    test "with invalid config" do
      assert {:error, :invalid_config} = LDAP.simple_bind({:ok, %{"some" => "stuff"}}, "user", "pass")
    end

    test "without SSL" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind({:ok, %{"host" => "localhost", "port" => 389}}, "user", "pass")
    end

    test "with regular SSL" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind({:ok, %{"host" => "localhost", "port" => 636, "encryption" => "ssl"}}, "user", "pass")
    end

    test "with StartTLS" do
      assert {:error, :invalid_credentials} =
               LDAP.simple_bind(
                 {:ok, %{"host" => "localhost", "port" => 389, "encryption" => "start_tls"}},
                 "user",
                 "pass"
               )
    end
  end
end
