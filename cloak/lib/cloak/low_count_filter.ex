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
  Records which properties were dropped during anonymization so we can later
  take these into account when generating the final low count properties property.
  """
  @spec record_dropped_property(t, Property.t) :: :ok
  def record_dropped_property({ets_table, _table_owner}, property) do
    :ets.insert(ets_table, {:lcf_property, property})
    :ok
  end

  @doc """
  Returns a list tuples indicating how many properties each returned user had filtered.
  This data can then be used to generate any low count filter property that is desired.
  """
  @spec filtered_property_counts(t) :: [{User.id, pos_integer}]
  def filtered_property_counts({ets_table, _table_owner}) do
    properties = :ets.lookup(ets_table, :lcf_property)
    |> Enum.map(fn({_, property}) -> property end)

    # We use this table to record how many properties each user has gotten suppressed
    user_count_table = :ets.new(__MODULE__,
      [:set, :public, {:write_concurrency, true}, {:read_concurrency, false}])

    try do
      processors = start_aggregate_processors(ets_table, user_count_table)
      dispatch_aggregations(properties, processors)
      stop_aggregate_processors(processors)

      # We can now generate a table of tuples {<user_id>, <dropped-property_count>}
      # that we can generate so the consumer can generate an lcf property based on it
      :ets.foldl(&([&1|&2]), [], user_count_table)
    after
      :ets.delete(user_count_table)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  def start_aggregate_processors(table, user_count_table) do
    for _ <- 1..@aggregate_processors do
      spawn_link(fn() -> aggregate_processor_loop(table, user_count_table) end)
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

  def aggregate_processor_loop(table, user_count_table) do
    receive do
      {:aggregate, property} ->
        for matches <- :ets.match(table, {property, :"$1"}),
            user_ids <- matches,
            user_id <- user_ids do
          :ets.update_counter(user_count_table, user_id, {2, 1}, {user_id, 0})
        end
        aggregate_processor_loop(table, user_count_table)
      :stop -> :ok
    end
  end
end
