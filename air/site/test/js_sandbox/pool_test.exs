defmodule Air.JsSandbox.PoolTest do
  use ExUnit.Case, async: true
  alias Air.JsSandbox.Pool

  test "calling an undefined function" do
    {:ok, pool} = Pool.start_link("test/js_sandbox/js")

    # We'll sum consecutive integers in JS and Elixir and compare results.
    test_range = 1..100
    expected_sum = test_range |> Enum.map(&(&1 * 2)) |> Enum.sum()
    sum_of_doubles = test_range
    # using Task.async to hopefully trigger concurrent executions of multiple workers.
    |> Enum.map(&Task.async(fn -> Pool.call(pool, "double_value", [&1]) end))
    |> Enum.map(&Task.await/1)
    |> Enum.map(fn({:ok, result}) -> result end)
    |> Enum.sum()

    assert sum_of_doubles == expected_sum

    # not really needed, but suppresses some logging noise
    ExUnit.CaptureLog.capture_log(fn -> GenServer.stop(pool); :timer.sleep(10) end)
  end
end
