defmodule Air.Service.Settings.Test do
  use Air.SchemaCase, async: false

  test "reading default settings" do
    assert Air.Service.Settings.read() ==
             %Air.Settings{
               query_retention_days: :unlimited,
               audit_log_enabled: true,
               decimal_digits: 3,
               decimal_sep: ".",
               thousand_sep: " "
             }
  end

  describe "update settings" do
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
  end
end
