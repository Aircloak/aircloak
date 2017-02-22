defmodule Cloak.MemoryReader.VMStatReader do
  @moduledoc """
  Module for reading the memory properties of vm_stat on macOS.
  """

  alias Cloak.MemoryReader.MemInfo


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses the results of a call to /usr/bin/vm_stat"
  @spec parse(String.t) :: MemInfo.t
  def parse(info) do
    page_size = run_regex(~r/page size of (\d+) bytes/, info)

    free_pages = run_regex(~r/Pages free:\s+(\d+)./, info)
    active_pages = run_regex(~r/Pages active:\s+(\d+)./, info)
    inactive_pages = run_regex(~r/Pages inactive:\s+(\d+)./, info)
    wired_down_pages = run_regex(~r/Pages wired down:\s+(\d+)./, info)

    total_memory_kb = trunc((free_pages + active_pages + inactive_pages + wired_down_pages) * page_size / 1024)
    # This is a little bit funky... We aren't really reporting "free" as in unoccupied,
    # but rather the amount of memory that could still be used by the cloak.
    available_memory_kb = trunc((free_pages + inactive_pages) * page_size / 1024)

    %MemInfo{total_memory: total_memory_kb, free_memory: available_memory_kb}
  end

  @doc "Reads the memory and returns a memory info struct"
  @spec read() :: MemInfo.t
  def read() do
    {data, 0} = System.cmd("/usr/bin/vm_stat", [])
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
