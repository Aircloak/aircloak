defmodule Air.Service.Cloak.Stats.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  alias Air.Service.Cloak.Stats

  @cloak_id "cloak_id"

  setup do
    clear_stats()
    :ok
  end

  describe "registration and removal" do
    test "implicitly registered on first metric" do
      refute Stats.cloak_stats(@cloak_id)
      Stats.record_query(@cloak_id)
      Stats.aggregate()
      assert Stats.cloak_stats(@cloak_id)
    end

    test "implicitly removed when not seen for a set of reporting intervals" do
      Stats.record_query(@cloak_id)
      Stats.aggregate()
      assert Stats.cloak_stats(@cloak_id)
      clear_stats()
      refute Stats.cloak_stats(@cloak_id)
    end
  end

  describe "recording stats" do
    test "records memory" do
      Stats.record_memory(@cloak_id, memory_reading())
      Stats.aggregate()
      assert %{memory: %{total: 100, readings: [60 | _]}} = Stats.cloak_stats(@cloak_id)
    end

    test "records queries" do
      Stats.record_query(@cloak_id)
      Stats.aggregate()
      assert %{queries: [1 | _]} = Stats.cloak_stats(@cloak_id)
    end
  end

  defp clear_stats() do
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
    Stats.aggregate()
  end

  defp memory_reading() do
    %{
      total_memory: 100,
      available_memory: %{
        current: 40,
        last_5_seconds: 40,
        last_1_minute: 40,
        last_5_minutes: 40,
        last_15_minutes: 40,
        last_1_hour: 40
      }
    }
  end
end
