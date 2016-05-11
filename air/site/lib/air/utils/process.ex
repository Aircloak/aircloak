defmodule Air.Utils.Process do
  def alive?(node_name, pid) do
    case :rpc.call(node_name, Process, :alive?, [pid]) do
      true -> true
      _ -> false
    end
  end
end
