defmodule Air.Service.Monitoring.Test do
  use ExUnit.Case, async: false

  alias Air.{Repo, Service.Monitoring, TestRepoHelper}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "assemble_info" do
    test "list of group names" do
      group = TestRepoHelper.create_group!()
      assert group.name in Monitoring.assemble_info().groups
    end
  end
end
