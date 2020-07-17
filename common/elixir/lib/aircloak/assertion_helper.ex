defmodule Aircloak.AssertionHelper do
  @moduledoc """
  Adds generic assertion useful in tests
  """

  @doc """
  You can essentially rewrite code that does:

      assert(soon(assertion))

  to

      assert_soon assertion

  Not only is this syntactically lighter, but you will also get nicer error messages in the test output.
  Assertions can also be pattern matches or anything else that `assert` normally expects.

  Usage:

      assert_soon {:ok, _} = Database.find_user(1)
  """
  defmacro assert_soon(assertion, opts \\ []) do
    quote do
      Aircloak.AssertionHelper.soon(unquote(opts), do: assert(unquote(assertion)))
    end
  end

  @doc """
  Rewrites `refute(soon(assertion))` to `refute_soon assertion` with the same benefits as `assert_soon`.
  """
  defmacro refute_soon(assertion, opts \\ []) do
    quote do
      Aircloak.AssertionHelper.soon(unquote(opts), do: refute(unquote(assertion)))
    end
  end

  @doc """
  Attempts to evaluate a condition until it becomes truthy.
  Returns a boolean indicating the truthness of the result.

  If a `do` block is passed, it will instead attempt to evaluate it until all assertions in the block pass.
  Returns the value of the last expression in the block.

  Retries up to a total of 10 attempts with a total timeout of `opts[:timeout]`,
  which is divided evenly between attempts.
  Default timeout is 200 ms, meaning 20ms between attempts.

  Usage:

      assert soon(Database.status() == :ok, timeout: :timer.seconds(2))
      assert soon(users_added?())

      soon do
        assert users_added?()
        assert {:ok, count} = Database.count_users()
        assert count == 5
      end
  """
  defmacro soon(opts, do: block) do
    quote do
      {attempts, repeat_wait_time} = Aircloak.AssertionHelper.compute_soon_attempts(unquote(opts))

      {_status, result} =
        Aircloak.AssertionHelper.perform_soon_check(
          fn -> {true, unquote(block)} end,
          attempts,
          repeat_wait_time
        )

      result
    end
  end

  defmacro soon(check, opts) do
    quote do
      {attempts, repeat_wait_time} = Aircloak.AssertionHelper.compute_soon_attempts(unquote(opts))

      {status, _result} =
        Aircloak.AssertionHelper.perform_soon_check(
          fn -> {unquote(check), nil} end,
          attempts,
          repeat_wait_time
        )

      status
    end
  end

  defmacro soon(do: block) do
    quote do
      Aircloak.AssertionHelper.soon([], do: unquote(block))
    end
  end

  defmacro soon(check) do
    quote do
      Aircloak.AssertionHelper.soon(unquote(check), [])
    end
  end

  def perform_soon_check(check, 1, _repeat_wait_time), do: check.()

  def perform_soon_check(check, remaining_attempts, repeat_wait_time) do
    {success, result} =
      try do
        check.()
      rescue
        ExUnit.AssertionError -> {false, nil}
      end

    if success do
      {true, result}
    else
      :timer.sleep(repeat_wait_time)
      perform_soon_check(check, remaining_attempts - 1, repeat_wait_time)
    end
  end

  def compute_soon_attempts(opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 200)
    repeat_wait_time = Keyword.get(opts, :repeat_wait_time, div(timeout, 10))
    attempts = trunc(Float.ceil(timeout / repeat_wait_time))
    {attempts, repeat_wait_time}
  end
end
