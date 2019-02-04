defmodule Aircloak.AssertionHelper do
  @moduledoc """
  Adds generic assertion useful in tests
  """

  @doc """
  Executed until it succeeds or up to a total of 10 attempts with a total timeout of `timeout`.

  Usage:

    assert soon(check_that_will_eventually_be_true())
    assert soon(check_that_will_eventually_be_true(), 1000) # Wait's up to a second
  """
  defmacro soon(check, timeout \\ 100, opts \\ []) do
    quote do
      repeat_wait_time = Keyword.get(unquote(opts), :repeat_wait_time, div(unquote(timeout), 10))

      Aircloak.AssertionHelper.perform_soon_check(
        fn -> unquote(check) end,
        trunc(Float.ceil(unquote(timeout) / repeat_wait_time)),
        repeat_wait_time
      )
    end
  end

  def perform_soon_check(_check, 0, _repeat_wait_time), do: false

  def perform_soon_check(check, remaining_attempts, repeat_wait_time) do
    if check.() do
      true
    else
      :timer.sleep(repeat_wait_time)
      perform_soon_check(check, remaining_attempts - 1, repeat_wait_time)
    end
  end
end
