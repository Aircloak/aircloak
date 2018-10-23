defmodule Air.Service.Cloak.Stats.Internal.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  alias Air.Service.Cloak.Stats

  @cloak_id "cloak_id"

  describe "initialize" do
    test "adds cloak to stats" do
      state = Stats.Internal.new()
      refute Map.has_key?(state.stats, @cloak_id)

      assert state
             |> Stats.Internal.initialize(@cloak_id)
             |> Map.get(:stats)
             |> Map.has_key?(@cloak_id)
    end

    test "adds dummy memory reading history" do
      assert 360 ==
               Stats.Internal.new()
               |> Stats.Internal.initialize(@cloak_id)
               |> get_mem_stat(:readings)
               |> Enum.count()

      assert Stats.Internal.new()
             |> Stats.Internal.initialize(@cloak_id)
             |> get_mem_stat(:readings)
             |> Enum.all?(&(&1 == 0))
    end

    test "adds dummy queries history" do
      assert 360 ==
               Stats.Internal.new()
               |> Stats.Internal.initialize(@cloak_id)
               |> get_queries_stat()
               |> Enum.count()

      assert Stats.Internal.new()
             |> Stats.Internal.initialize(@cloak_id)
             |> get_queries_stat()
             |> Enum.all?(&(&1 == 0))
    end
  end

  describe "remove" do
    test "removes cloak from stats" do
      refute initialized_state()
             |> Stats.Internal.remove(@cloak_id)
             |> Stats.Internal.cloak_stats()
             |> Map.keys()
             |> Enum.member?(@cloak_id)
    end

    test "removes pending data from state" do
      assert initialized_state()
             |> Stats.Internal.record_memory(@cloak_id, memory_reading())
             |> Stats.Internal.remove(@cloak_id)
             |> get_in([:pending_memory_readings, @cloak_id])
             |> is_nil()
    end

    test "removes pending queries from state" do
      assert initialized_state()
             |> Stats.Internal.record_query(@cloak_id)
             |> Stats.Internal.remove(@cloak_id)
             |> get_in([:pending_queries, @cloak_id])
             |> is_nil()
    end
  end

  describe "record_query" do
    test "adds query to queue for later aggregation" do
      assert 2 ==
               initialized_state()
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.record_query(@cloak_id)
               |> get_in([:pending_queries, @cloak_id])
    end
  end

  describe "record_memory" do
    test "records basic memory stats" do
      state =
        initialized_state()
        |> Stats.Internal.record_memory(@cloak_id, memory_reading())
        |> Stats.Internal.aggregate()

      assert 100 == get_mem_stat(state, :total)
      assert 60 == get_mem_stat(state, :currently_in_use)
      assert 60 == get_mem_stat(state, :in_use_percent)
    end

    test "adds reading to queue for later aggregation" do
      assert [memory_reading(), memory_reading()] ==
               initialized_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> get_in([:pending_memory_readings, @cloak_id])
    end
  end

  describe "aggregate" do
    test "adds maximum in usage percentage to readings" do
      assert 100 ==
               initialized_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.record_memory(@cloak_id, other_memory_reading())
               |> Stats.Internal.aggregate()
               |> get_mem_stat(:readings)
               |> List.first()
    end

    test "adds pending queries to the run queries" do
      assert [1, 2] =
               initialized_state()
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.aggregate()
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.aggregate()
               |> get_queries_stat()
               |> Enum.take(2)
    end
  end

  describe "cloak_stats" do
    test "returns stats for all cloaks" do
      assert ["cloak1", "cloak2"] ==
               Stats.Internal.new()
               |> Stats.Internal.initialize("cloak1")
               |> Stats.Internal.initialize("cloak2")
               |> Stats.Internal.record_memory("cloak1", memory_reading())
               |> Stats.Internal.record_memory("cloak2", memory_reading())
               |> Stats.Internal.cloak_stats()
               |> Map.keys()
    end

    test "returns latest memory reading" do
      assert %{@cloak_id => stats} =
               initialized_state()
               |> Stats.Internal.record_memory(@cloak_id, memory_reading())
               |> Stats.Internal.aggregate()
               |> Stats.Internal.cloak_stats()

      assert 100 == stats.memory.total
      assert 60 == List.first(stats.memory.readings)
    end

    test "repeats last memory reading if no new has arrived" do
      initial_state =
        initialized_state()
        |> Stats.Internal.record_memory(@cloak_id, memory_reading())
        |> Stats.Internal.aggregate()

      initial_reading = initial_state |> get_mem_stat(:readings) |> List.first()

      subsequent_reading =
        initial_state
        |> Stats.Internal.aggregate()
        |> get_mem_stat(:readings)
        |> List.first()

      assert initial_reading == subsequent_reading
    end

    test "returns latest queries" do
      assert %{@cloak_id => stats} =
               initialized_state()
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.aggregate()
               |> Stats.Internal.cloak_stats()

      assert 1 == List.first(stats.queries)
    end

    test "returns 0 for queries if none have been run in last period" do
      assert %{@cloak_id => stats} =
               initialized_state()
               |> Stats.Internal.record_query(@cloak_id)
               |> Stats.Internal.aggregate()
               |> Stats.Internal.aggregate()
               |> Stats.Internal.cloak_stats()

      assert 0 == List.first(stats.queries)
    end
  end

  defp initialized_state(),
    do:
      Stats.Internal.new()
      |> Stats.Internal.initialize(@cloak_id)

  defp get_mem_stat(stats, key) do
    get_in(stats, [:stats, @cloak_id, :memory, key])
  end

  defp get_queries_stat(stats) do
    get_in(stats, [:stats, @cloak_id, :queries])
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
