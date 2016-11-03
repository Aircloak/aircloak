defmodule Central.Utils.Process do
  @moduledoc """
  Utilities for working with processes
  """

  @doc """
  Checks if process `pid` is alive. Works just like `Process.alive?` but also for remote processes.
  """
  @spec alive?(pid :: pid) :: boolean
  def alive?(pid) do
    case :rpc.call(node(pid), Process, :alive?, [pid], :timer.seconds(1)) do
      true -> true
      _ -> false
    end
  end
end
