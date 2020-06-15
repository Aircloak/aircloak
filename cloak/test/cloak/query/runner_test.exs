defmodule Cloak.Query.RunnerTest do
  use ExUnit.Case, async: false
  require Aircloak.AssertionHelper
  alias Cloak.Query.Runner

  describe "unbound query concurrency" do
    setup do
      restart_runner_supervisor()
    end

    test "can start multiple concurrent runners" do
      Enum.each(1..100, fn _ -> assert {:ok, _query_id} = start_runner() end)
    end
  end

  describe "bound query concurrency" do
    setup do
      restart_runner_supervisor()
      Application.put_env(:cloak, :max_parallel_queries, 2)
      on_exit(fn -> Application.delete_env(:cloak, :max_parallel_queries) end)
    end

    test "error on too many queries" do
      assert {:ok, _query_id} = start_runner()
      assert {:ok, _query_id} = start_runner()
      assert {:error, :too_many_queries} = start_runner()
    end

    test "query count is updated when the query stops" do
      assert {:ok, _query_id} = start_runner()
      assert {:ok, _query_id} = start_runner()

      assert_receive {:get_runner_result, runner_pid}
      send(runner_pid, {:runner_result, {:error, "query error"}})
      assert_receive {:result, _result}

      assert Aircloak.AssertionHelper.soon(match?({:ok, _query_id}, start_runner()))
    end

    test "query count is updated when the query process crashes" do
      assert {:ok, _query_id} = start_runner()
      assert {:ok, _query_id} = start_runner()

      assert_receive {:get_runner_result, runner_pid}
      Process.exit(runner_pid, :kill)
      assert_receive {:result, _result}

      assert Aircloak.AssertionHelper.soon(match?({:ok, _query_id}, start_runner()))
    end
  end

  defp restart_runner_supervisor() do
    Supervisor.terminate_child(Cloak.Supervisor, Runner)
    Supervisor.restart_child(Cloak.Supervisor, Runner)
    :ok
  end

  defp start_runner() do
    query_id = :erlang.unique_integer()

    with :ok <- Runner.start(query_id, nil, nil, nil, nil, nil, nil, result_target: self(), runner_fun: runner_fun()),
         do: {:ok, query_id}
  end

  defp runner_fun() do
    test_pid = self()

    fn _runner_args ->
      send(test_pid, {:get_runner_result, self()})

      receive do
        {:runner_result, result} -> result
      end
    end
  end
end
