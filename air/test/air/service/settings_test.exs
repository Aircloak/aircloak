defmodule Air.Service.Settings.Test do
  use ExUnit.Case, async: false

  alias Air.Repo

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Repo.delete_all(Air.Schemas.Settings)
    :ok
  end

  test "reading default settings" do
    assert Air.Service.Settings.read() ==
             %Air.Settings{
               query_retention_days: :unlimited,
               audit_log_enabled: true,
               decimal_digits: 3,
               decimal_sep: ".",
               thousand_sep: " ",
               ldap_host: "",
               ldap_port: nil,
               ldap_ssl: false,
               ldap_ca_cert: nil
             }
  end

  describe ".save" do
    test "set retention" do
      Air.Service.Settings.save(%{"query_retention_days" => 120})
      assert Air.Service.Settings.read().query_retention_days == 120
    end

    test "set and unset retention" do
      Air.Service.Settings.save(%{"query_retention_days" => 120})
      Air.Service.Settings.save(%{"query_retention_days" => nil})
      assert Air.Service.Settings.read().query_retention_days == :unlimited
    end

    test "disable audit log" do
      Air.Service.Settings.save(%{"audit_log_enabled" => false})
      assert Air.Service.Settings.read().audit_log_enabled == false
    end

    test "cannot set LDAP settings" do
      Air.Service.Settings.save(%{"ldap_host" => "new_host"})
      assert Air.Service.Settings.read().ldap_host == ""
    end
  end

  describe ".save_ldap" do
    test "cannot set non-LDAP settings" do
      Air.Service.Settings.save_ldap(%{"query_retention_days" => 120})
      assert Air.Service.Settings.read().query_retention_days == :unlimited
    end

    test "can set LDAP settings" do
      Air.Service.Settings.save_ldap(%{"ldap_port" => 389})
      assert Air.Service.Settings.read().ldap_port == 389
    end
  end
end
