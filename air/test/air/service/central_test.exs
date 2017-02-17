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
      |> Enum.map(&(&1.event))
      |> Enum.sort()
  end

  test "deleting a call" do
    {:ok, _} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, ev2} = Central.store_pending_call("event2", %{another: "payload"})
    Central.remove_pending_call!(ev2)
    assert ["event1"] == Enum.map(Central.pending_calls(), &(&1.event))
  end

  test "exporting pending calls" do
    {:ok, _} = Central.store_pending_call("event1", %{some: "payload"})
    {:ok, _} = Central.store_pending_call("event2", %{another: "payload"})
    assert {:ok, _} = Central.export_pending_calls()
    assert [] == Central.pending_calls()
  end

  test "export error when there are no pending calls" do
    assert {:error, :nothing_to_export} == Central.export_pending_calls()
  end
end
