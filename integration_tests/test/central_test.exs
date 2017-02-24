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

  describe "manual export" do
    setup do
      Application.put_env(:air, :auto_aircloak_export, false)
      poll_for_export()
      Air.Repo.delete_all(Air.Schemas.ExportForAircloak)
      Central.Repo.delete_all(Central.Schemas.CustomerExport)
      on_exit(fn -> Application.put_env(:air, :auto_aircloak_export, true) end)
      :ok
    end

    test "single export" do
      assert {:ok, number} = import_to_central(poll_for_export())
      assert number > 0
    end

    test "two exports" do
      {:ok, _} = import_to_central(poll_for_export())
      assert {:ok, _} = import_to_central(poll_for_export())
    end

    test "duplicate export" do
      export = poll_for_export()
      {:ok, _} = import_to_central(export)
      assert {:error, :already_imported} == import_to_central(export)
    end

    test "missing first export" do
      _export1 = poll_for_export()
      export2 = poll_for_export()
      assert {:error, {:missing_previous_export, nil}} == import_to_central(export2)
    end

    test "missing second export" do
      export1 = poll_for_export()
      import_to_central(export1)
      _export2 = poll_for_export()
      export3 = poll_for_export()
      assert {:error, {:missing_previous_export, export1.inserted_at}} == import_to_central(export3)
    end

    defp import_to_central(export), do:
      export
      |> Air.Schemas.ExportForAircloak.content()
      |> Central.Service.Customer.Message.import_customer_data()

    defp poll_for_export() do
      case Air.Service.Central.export_pending_calls() do
        {:ok, export} -> export
        {:error, :nothing_to_export} ->
          :timer.sleep(10)
          poll_for_export()
      end
    end
  end

  defp air(), do:
    Enum.find(
      Central.Service.Customer.airs(),
      &(&1.customer.name == "integration tests customer")
    )
end
