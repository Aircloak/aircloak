defmodule Air.Utils.Process do
  @moduledoc """
  Utilities for working with processes
  """

  @timeout :timer.seconds(1)

  @doc """
  Checks if process `pid` is alive. Works just like `Process.alive?` but also for remote processes.
  """
  @spec alive?(pid :: pid) :: boolean
  def alive?(pid) do
    case :rpc.call(node(pid), Process, :alive?, [pid], @timeout) do
      true -> true
      _ -> false
    end
  end
end
