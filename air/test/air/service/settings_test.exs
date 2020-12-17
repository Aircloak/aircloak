defmodule Air.Service.Settings.Test do
  use Air.SchemaCase, async: false

  setup do
    Air.Repo.delete_all(Air.Schemas.Settings)
    {:ok, server} = GenServer.start_link(Air.Service.Settings, nil)
    {:ok, server: server}
  end

  test "reading default settings", %{server: server} do
    assert Air.Service.Settings.read(server) ==
             %Air.Settings{
               query_retention_days: :unlimited,
               audit_log_enabled: true,
               decimal_digits: 3,
               decimal_sep: ".",
               thousand_sep: " ",
               type_checking_enabled: true
             }
  end

  describe "update settings" do
    test "set retention", %{server: server} do
      Air.Service.Settings.save(server, %{"query_retention_days" => 120})
      assert Air.Service.Settings.read(server).query_retention_days == 120
    end

    test "set and unset retention", %{server: server} do
      Air.Service.Settings.save(server, %{"query_retention_days" => 120})
      Air.Service.Settings.save(server, %{"query_retention_days" => nil})
      assert Air.Service.Settings.read(server).query_retention_days == :unlimited
    end

    test "disable audit log", %{server: server} do
      Air.Service.Settings.save(server, %{"audit_log_enabled" => false})
      assert Air.Service.Settings.read(server).audit_log_enabled == false
    end
  end
end
