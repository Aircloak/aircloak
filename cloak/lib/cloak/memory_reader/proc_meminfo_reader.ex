defmodule Cloak.MemoryReader.ProcMeminfoReader do
  @moduledoc """
  Module for reading the memory properties of a Linux machine.
  """

  alias Cloak.MemoryReader.MemInfo


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses the results of a call to /proc/meminfo"
  @spec parse(String.t) :: MemInfo.t
  def parse(info) do
    total_memory = run_regex(~r/MemTotal:\s+(\d+) kB/, info)
    free_memory = run_regex(~r/MemFree:\s+(\d+) kB/, info)
    %MemInfo{total_memory: total_memory, free_memory: free_memory}
  end

  @doc "Reads the memory and returns a memory info struct"
  @spec read() :: MemInfo.t
  def read() do
    {data, 0} = System.cmd("cat", ["/proc/meminfo"])
    parse(data)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_regex(regex, string), do:
    Regex.run(regex, string, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> hd()
end
