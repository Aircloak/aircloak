defmodule Air.Service.Settings.Test do
  use ExUnit.Case, async: false

  alias Air.Repo

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Repo.delete_all(Air.Schemas.Settings)
    :ok
  end

  test "reading default settings" do
    assert Air.Service.Settings.read() == %Air.Settings{query_retention_days: :unlimited, audit_log_enabled: true}
  end

  describe "update settings" do
    test "set retention" do
      Air.Service.Settings.update(_user = nil, params(%{query_retention_days: 120}))
      assert Air.Service.Settings.read().query_retention_days == 120
    end

    test "set and unset retention" do
      Air.Service.Settings.update(_user = nil, params(%{query_retention_days: 120}))
      Air.Service.Settings.update(_user = nil, params(%{query_retention_days: :unlimited}))
      assert Air.Service.Settings.read().query_retention_days == :unlimited
    end

    test "disable audit log" do
      Air.Service.Settings.update(_user = nil, params(%{audit_log_enabled: false}))
      assert Air.Service.Settings.read().audit_log_enabled == false
    end
  end

  defp params(values), do:
    Map.merge(%{query_retention_days: :unlimited, audit_log_enabled: true}, values)
end
