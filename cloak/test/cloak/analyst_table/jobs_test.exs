defmodule Cloak.AnalystTable.JobsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import StreamData
  alias Cloak.AnalystTable.Jobs

  property "jobs are processed in the proper order" do
    check all(
            operations <- list_of(operation()),
            processed_jobs = process_all_jobs(operations)
          ) do
      assert processed_jobs == input_jobs(operations)
    end
  end

  property "no more than five jobs are running at the same time" do
    check all(operations <- list_of(operation())) do
      operations
      |> all_states()
      |> Enum.each(&assert(length(&1.jobs.running) <= 5))
    end
  end

  defp input_jobs(operations),
    do: operations |> Stream.filter(&match?({:store_job, _job}, &1)) |> Enum.map(fn {:store_job, job} -> job end)

  defp all_states(operations) do
    Stream.concat([initial_state()], Stream.scan(operations, initial_state(), &apply_operation/2))
  end

  defp process_all_jobs(operations) do
    state = operations |> all_states() |> Enum.at(-1) |> drain()
    List.flatten(state.processed_jobs)
  end

  defp initial_state(), do: %{jobs: Jobs.new(), running_jobs: [], processed_jobs: []}

  defp apply_operation({:store_job, job}, state), do: update_in(state.jobs, &Jobs.enqueue_job(&1, job))

  defp apply_operation(:next_jobs, state) do
    {taken_jobs, jobs} = Jobs.next_jobs(state.jobs)
    %{state | jobs: jobs, running_jobs: state.running_jobs ++ taken_jobs}
  end

  defp apply_operation(:job_finished, state) do
    jobs = Enum.reduce(state.running_jobs, state.jobs, &Jobs.job_finished(&2, &1))
    %{state | jobs: jobs, running_jobs: [], processed_jobs: [state.processed_jobs, state.running_jobs]}
  end

  defp drain(state) do
    case apply_operation(:next_jobs, state) do
      %{running_jobs: []} = state -> state
      state -> drain(apply_operation(:job_finished, state))
    end
  end

  defp operation() do
    frequency([
      {3, {:store_job, create_table_job()}},
      {2, :next_jobs},
      {1, :job_finished}
    ])
  end

  defp create_table_job(), do: {:create_table, table()}

  defp table() do
    fixed_map(%{
      analyst: non_empty_string(),
      name: non_empty_string(),
      statement: non_empty_string(),
      data_source_name: non_empty_string(),
      db_name: non_empty_string(),
      id_column: nil,
      store_info: non_empty_string(),
      columns: nonempty(list_of(column(), max_length: 10))
    })
  end

  defp column() do
    fixed_map(%{
      name: non_empty_string(),
      access: member_of(~w[visible, unselectable, hidden]a),
      type: member_of(~w[text, integer, real, boolean, datetime, time, date, interval, unknown]a)
    })
  end

  defp non_empty_string(), do: string(:ascii, min_length: 1, max_length: 10)
end
