defmodule Air.Utils.Process do
  @moduledoc """
  Utilities for working with processes
  """

  @doc """
  Checks if process `pid` on node `node_name` is alive. Works just like `Process.alive?` but also for remote
  processes.
  """
  @spec alive?(node_name :: atom, pid :: pid) :: boolean
  def alive?(node_name, pid) do
    case :rpc.call(node_name, Process, :alive?, [pid]) do
      true -> true
      _ -> false
    end
  end
end
