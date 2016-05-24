defmodule Cloak.LowCountFilter do
  @moduledoc """
  This module accounts for properties that are low count filtered during anonymization.

  It can be used for any number of anonymizers.
  Once all other properties have been processed and anonymized, this module can be
  used to generate a reportable and anonymous property accounting for how many properties
  were filtered out.
  """

  require Logger

  alias Cloak.Property

  @type t :: {:ets.tab, pid}

  @aggregate_processors 10
  @lcf_property "aircloak_lcf_tail"


  ## -----------------------------------------------------------------
  ## API functions
  ## -----------------------------------------------------------------

  @doc "Creates the new lcf users storage."
  @spec new() :: t
  def new do
    caller = self()
    # Spawn the table owner
    table_owner = spawn(fn() ->
      # Using duplicate_bag to improve performance. Since each user is handled in a dedicated partition,
      # overlapping entries shouldn't happen. Moreover, aggregation is used at the output to deduplicate entries,
      # so we can safely use a more performant version here to speed up insertions.
      ets_table = :ets.new(__MODULE__, [:duplicate_bag, :public, {:write_concurrency, true}, {:read_concurrency, true}])
      send(caller, {:ets_table, ets_table})
      # Monitor the master so we can terminate if it goes down
      mref = Process.monitor(caller)
      receive do
        :stop -> :ok
        {:DOWN, ^mref, :process, _, _} ->
          Logger.info("Master process down, stopping the local LowCountFilter's table owner")
          :ok
      end
    end)
    receive do
      {:ets_table, ets_table} -> {ets_table, table_owner}
      after 5000 ->
        # Shouldn't happen, but just in case
        :erlang.error(:timeout)
    end
  end

  @doc "Deletes the storage."
  @spec delete(t) :: :ok
  def delete({_ets_table, table_owner}) do
    send(table_owner, :stop)
    :ok
  end

  @doc "Adds `property() -> [user_id()]' mapping to the storage."
  @spec add_bucket_users(t, Property.t, [User.id]) :: t
  def add_bucket_users({ets_table, _table_owner} = lcf_storage, property, user_ids) do
    :ets.insert(ets_table, {property, user_ids})
    lcf_storage
  end

  @doc """
  Generates the lcf_tail report based on the data in the storage.
  The generated property will be correctly connected to the sorted list of
  unique users which are lcf-ed. This function is not meant to be invoked
  directly. It will be called by the anonymizer after the reported properties
  have been lcf-ed.
  """
  @spec lcf_tail_report(t, [Property.t], pos_integer) :: :undefined | Bucket.t
  def lcf_tail_report(_, [], _), do: :undefined
  def lcf_tail_report({ets_table, _table_owner}, properties, columns_count) do
    aggregator = :aggregator.new()
    try do
      processors = start_aggregate_processors(ets_table, columns_count, aggregator)
      dispatch_aggregations(properties, processors)
      stop_aggregate_processors(processors)
      case :aggregator.buckets(aggregator) do
        [] -> :undefined
        [lcf_report] -> lcf_report
      end
    after
      :aggregator.delete(aggregator)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  def start_aggregate_processors(table, columns_count, aggregator) do
    for _ <- 1..@aggregate_processors do
      spawn_link(fn() -> aggregate_processor_loop(table, columns_count, aggregator) end)
    end
  end

  def dispatch_aggregations(properties, processors) do
    dispatch_aggregations(properties, [], processors)
  end

  # We're dispatching each bucket to one processor in a round-robin fashion.
  def dispatch_aggregations([], _, _), do: :ok
  def dispatch_aggregations(properties, [], all_processors) do
    dispatch_aggregations(properties, all_processors, all_processors)
  end
  def dispatch_aggregations([property | rest_properties], [processor_pid | rest_processors], all_processors) do
    send(processor_pid, {:aggregate, property})
    dispatch_aggregations(rest_properties, rest_processors, all_processors)
  end

  def stop_aggregate_processors(processors) do
    mrefs = for processor_pid <- processors do
      send(processor_pid, :stop)
      Process.monitor(processor_pid)
    end
    # Wait for processes to finish, and then collect the final report.
    for mref <- mrefs do
      receive do
        {:DOWN, ^mref, :process, _, _} -> :ok
      end
    end
    :ok
  end

  def aggregate_processor_loop(table, columns_count, aggregator) do
    receive do
      {:aggregate, property} ->
        for matches <- :ets.match(table, {property, :"$1"}),
            user_ids <- matches,
            user_id <- user_ids do
          :aggregator.add_property(List.duplicate(@lcf_property, columns_count), user_id, aggregator)
        end
        aggregate_processor_loop(table, columns_count, aggregator)
      :stop -> :ok
    end
  end
end
