defmodule Air.Service.CentralTest do
  use Air.SchemaCase, async: false

  alias Air.{Repo, Service.Central, Schemas.CentralCall}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    Repo.delete_all(CentralCall)
    :ok
  end

  test "storing pending calls" do
    assert {:ok, call} = Central.store_pending_call("event1", %{some: "payload"})
    assert call.event == "event1"
    assert call.payload == %{some: "payload"}
  end

  test "retrieving calls" do
    {:ok, _} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, _} = Central.store_pending_call("event2", %{another: "payload"})

    assert ["event1", "event2"] ==
             Central.pending_calls()
             |> Enum.map(& &1.event)
             |> Enum.sort()
  end

  test "exporting pending calls" do
    {:ok, _} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, _} = Central.store_pending_call("event2", %{another: "payload"})
    assert {:ok, _} = Central.export_pending_calls()
    assert [] == Central.pending_calls()
  end

  test "retrieving an export calls" do
    {:ok, _} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, _} = Central.store_pending_call("event2", %{another: "payload"})
    {:ok, export} = Central.export_pending_calls()
    assert export == Central.get_export!(export.id)
  end

  test "export error when there are no pending calls" do
    assert {:error, :nothing_to_export} == Central.export_pending_calls()
  end

  test "oldest pending call time" do
    assert nil == Central.oldest_pending_call_time()

    {:ok, oldest_entry} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, _} = Central.store_pending_call("event2", %{another: "payload"})
    assert oldest_entry.inserted_at == Central.oldest_pending_call_time()
  end
end
