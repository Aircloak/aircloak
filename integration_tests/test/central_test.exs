defmodule IntegrationTest.CentralTest do
  use ExUnit.Case, async: false

  describe "manual export" do
    setup do
      Application.put_env(:air, :auto_aircloak_export, false)
      start_inserting_exportable_calls()
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
      |> Central.Service.Customer.import_customer_data()

    defp poll_for_export() do
      case Air.Service.Central.export_pending_calls() do
        {:ok, export} -> export
        {:error, :nothing_to_export} ->
          :timer.sleep(10)
          poll_for_export()
      end
    end
  end

  defp start_inserting_exportable_calls(), do:
    Task.start_link(fn -> create_exportable_call() end)

  defp create_exportable_call() do
    receive do
      :stop -> :ok
    after 0 ->
      %Air.Schemas.CentralCall{}
      |> Air.Schemas.CentralCall.changeset(%{event: "test event", payload: %{}})
      |> Air.Repo.insert!()
      :timer.sleep(10)
      create_exportable_call()
    end
  end
end
