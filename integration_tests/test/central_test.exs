defmodule IntegrationTest.CentralTest do
  use ExUnit.Case, async: false

  test "air status is stored in database" do
    assert air().status == :online
    assert length(air().cloaks) == 1
    assert hd(air().cloaks).name == hd(Air.DataSourceManager.cloaks()).name
    assert hd(air().cloaks).status == :online

    Supervisor.terminate_child(Air.Supervisor, Air.CentralSocket)
    :timer.sleep(100)
    assert air().status == :offline
    assert hd(air().cloaks).status == :offline

    Supervisor.restart_child(Air.Supervisor, Air.CentralSocket)
    :timer.sleep(100)
    assert air().status == :online
    assert hd(air().cloaks).status == :online
  end

  defp air(), do:
    Enum.find(
      Central.Service.Customer.airs(),
      &(&1.customer.name == "integration tests customer")
    )
end
