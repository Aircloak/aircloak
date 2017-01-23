defmodule Air.Service.Settings.Test do
  use ExUnit.Case, async: false

  alias Air.Repo

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Repo.delete_all(Air.Schemas.Settings)
    :ok
  end

  test "reading default settings" do
    assert Air.Service.Settings.read() == %Air.Settings{query_retention_days: :unlimited}
  end

  test "updating a setting" do
    Air.Service.Settings.update(%{query_retention_days: 120})
    assert Air.Service.Settings.read() == %Air.Settings{query_retention_days: 120}
  end
end
