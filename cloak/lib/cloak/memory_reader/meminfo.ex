defmodule Cloak.MemoryReader.MemInfo do
  @moduledoc """
  Allows parsing the information returned when reading /proc/meminfo
  on a linux machine.

  All memory values returned are in kB.
  """

  @type t :: %{
    total_memory: non_neg_integer,
    free_memory: non_neg_integer,
  }

  defstruct total_memory: 0, free_memory: 0


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses the results of a call to /proc/meminfo"
  @spec parse(String.t) :: t
  def parse(info) do
    total_memory = run_regex(~r/MemTotal:\s+(\d+) kB/, info)
    free_memory = run_regex(~r/MemFree:\s+(\d+) kB/, info)
    %__MODULE__{total_memory: total_memory, free_memory: free_memory}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_regex(regex, string), do:
    Regex.run(regex, string, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> hd()
end
