defmodule Air.Service.Cloak.Stats.Internal.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  alias Air.Service.Cloak.Stats

  @cloak_id "cloak_id"

  describe "record_memory" do
    test "adds cloak to stats" do
      state = initial_state()
      refute Map.has_key?(state.stats, @cloak_id)

      assert state
             |> Stats.Internal.record_memory(@cloak_id, memory_reading())
             |> Map.get(:stats)
             |> Map.has_key?(@cloak_id)
    end

    test "records basic memory stats" do
      state = Stats.Internal.record_memory(initial_state(), @cloak_id, memory_reading())

      assert 100 == get_mem_stat(state, :total)
      assert 60 == get_mem_stat(state, :currently_in_use)
      assert 60 == get_mem_stat(state, :in_use_percent)
    end

    test "updates changes base stats" do
      state = Stats.Internal.record_memory(initial_state(), @cloak_id, memory_reading())
      updated_state = Stats.Internal.record_memory(state, @cloak_id, other_memory_reading())

      Enum.each([:total, :currently_in_use, :in_use_percent], fn stat ->
        refute get_mem_stat(state, stat) == get_mem_stat(updated_state, stat)
      end)
    end

    test "adds dummy memory reading history" do
      assert 360 ==
               initial_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> get_mem_stat(:readings)
               |> Enum.count()

      assert initial_state()
             |> Stats.Internal.record_memory(@cloak_id, memory_reading())
             |> get_mem_stat(:readings)
             |> Enum.all?(&(&1 == 0))
    end

    test "adds reading to queue for later processing" do
      assert [memory_reading(), memory_reading()] ==
               initial_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> get_in([:pending_memory_readings, @cloak_id])
    end
  end

  describe "process" do
    test "noop when no new measurements" do
      state =
        initial_state()
        |> Stats.Internal.record_memory(@cloak_id, memory_reading())
        |> Stats.Internal.process()

      assert state == Stats.Internal.process(state)
    end

    test "adds maximum in usage percentage to readings" do
      assert 100 ==
               initial_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.record_memory(@cloak_id, other_memory_reading())
               |> Stats.Internal.process()
               |> get_mem_stat(:readings)
               |> List.first()
    end
  end

  describe "cloak_stats" do
    test "returns stats for all cloaks" do
      assert ["cloak1", "cloak2"] ==
               initial_state()
               |> Stats.Internal.record_memory("cloak1", memory_reading())
               |> Stats.Internal.record_memory("cloak2", memory_reading())
               |> Stats.Internal.cloak_stats()
               |> Map.keys()
    end

    test "returns latest memory reading" do
      assert %{@cloak_id => stats} =
               initial_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.process()
               |> Stats.Internal.cloak_stats()

      assert 100 == stats.memory.total
      assert 60 == List.first(stats.memory.readings)
    end
  end

  defp initial_state(), do: Stats.Internal.initial_state()

  defp get_mem_stat(stats, key) do
    get_in(stats, [:stats, @cloak_id, :memory, key])
  end

  defp other_memory_reading() do
    memory_reading()
    |> put_in([:total_memory], 200)
    |> put_in([:available_memory, :current], 0)
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
