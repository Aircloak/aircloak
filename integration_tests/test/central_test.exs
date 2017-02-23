defmodule IntegrationTest.CentralTest do
  use ExUnit.Case, async: false

  test "air status is stored in database" do
    assert air().status == :online
    assert length(air().cloaks) == 1
    assert hd(air().cloaks).name == hd(Air.DataSourceManager.cloaks()).name
    assert hd(air().cloaks).status == :online
    assert hd(air().cloaks).data_source_names == [IntegrationTest.Manager.data_source_global_id()]

    Supervisor.terminate_child(Air.Supervisor, Air.CentralClient)
    :timer.sleep(100)
    assert air().status == :offline
    assert hd(air().cloaks).status == :offline
    assert hd(air().cloaks).data_source_names == [IntegrationTest.Manager.data_source_global_id()]

    Supervisor.restart_child(Air.Supervisor, Air.CentralClient)
    :timer.sleep(100)
    assert air().status == :online
    assert hd(air().cloaks).status == :online
  end

  test "usage info is sent" do
    import Ecto.Query, only: [from: 2]

    Central.Repo.delete_all("usage_info")
    :timer.sleep(Application.fetch_env!(:air, :usage_report_interval) + 100)

    assert air().id == Central.Repo.one(
      from u in "usage_info",
      select: u.air_id,
      order_by: [desc: u.id],
      limit: 1
    )
  end

  test "manual export to central" do
    Application.put_env(:air, :auto_aircloak_export, false)
    export = poll_for_export()
    assert {:ok, number} = Central.CustomerMessage.import_customer_data(export.payload)
    assert number > 0
  end

  defp poll_for_export() do
    case Air.Service.Central.export_pending_calls() do
      {:ok, export} -> export
      {:error, :nothing_to_export} ->
        :timer.sleep(10)
        poll_for_export()
    end
  end

  defp air(), do:
    Enum.find(
      Central.Service.Customer.airs(),
      &(&1.customer.name == "integration tests customer")
    )
end
