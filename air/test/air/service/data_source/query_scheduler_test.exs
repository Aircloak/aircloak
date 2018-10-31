defmodule Air.Service.DataSource.QuerySchedulerTest do
  use ExUnit.Case, async: true
  alias Air.Service.DataSource.QueryScheduler

  test "runner is started on notification" do
    scheduler = start_scheduler!()
    QueryScheduler.notify(scheduler)

    assert_receive {:runner_started, _runner_pid}
  end

  test "runner is not restarted when there's no subsequent notification" do
    scheduler = start_scheduler!()
    QueryScheduler.notify(scheduler)

    assert_receive {:runner_started, _runner_pid}
    refute_receive {:runner_started, _runner_pid}
  end

  test "new runner is started on new notification after the current runner stops" do
    scheduler = start_scheduler!()
    QueryScheduler.notify(scheduler)

    assert_receive {:runner_started, runner_pid}

    QueryScheduler.notify(scheduler)
    refute_receive {:runner_started, _runner_pid}

    send(runner_pid, :continue)
    assert_receive {:runner_started, _runner_pid}
  end

  test "multiple notifications while the runner is running are collapsed" do
    scheduler = start_scheduler!()
    QueryScheduler.notify(scheduler)

    assert_receive {:runner_started, runner_pid}

    QueryScheduler.notify(scheduler)
    QueryScheduler.notify(scheduler)
    QueryScheduler.notify(scheduler)
    send(runner_pid, :continue)

    assert_receive {:runner_started, _runner_pid}
    refute_receive {:runner_started, _runner_pid}
  end

  defp start_scheduler!() do
    test_process = self()

    runner = fn ->
      send(test_process, {:runner_started, self()})

      receive do
        :continue -> :ok
      end
    end

    {:ok, pid} = QueryScheduler.start_link(name: nil, runner: runner)
    pid
  end
end
