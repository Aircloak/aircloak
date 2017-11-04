defmodule Cloak.MemoryReader.ProcMemInfo do
  @moduledoc """
  Allows parsing the information returned when reading /proc/meminfo on a linux machine.
  All memory values returned are in kB.
  """

  @type t :: %__MODULE__{
    total_memory: non_neg_integer,
    available_memory: non_neg_integer,
  }

  defstruct total_memory: 0, available_memory: 0


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Reads the memory and returns a memory info struct"
  @spec read() :: ProcMemInfo.t
  def read() do
    case File.open("/proc/meminfo", [:read, :binary], &IO.read(&1, :all)) do
      {:ok, data} -> parse(data)
      {:error, _} ->
        # We are on a system that doesn't have /proc/meminfo.
        # Fall back on using a defensive version of the erlang provided readings.
        reading = :memsup.get_system_memory_data()
        # The values we get back from memsup are in kB, whereas the values
        # provided by meminfo are in kB.
        total_memory = div(Keyword.get(reading, :total_memory, 0), 1024)
        free_memory = div(Keyword.get(reading, :free_memory, 0), 1024)
        %__MODULE__{total_memory: total_memory, available_memory: free_memory}
    end
  end

  @doc "Parses the results of a call to /proc/meminfo"
  @spec parse(String.t) :: t
  def parse(info) do
    total_memory = run_regex(~r/MemTotal:\s+(\d+) kB/, info)
    available_memory = run_regex(~r/MemAvailable:\s+(\d+) kB/, info)
    %__MODULE__{total_memory: total_memory, available_memory: available_memory}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_regex(regex, string), do:
    Regex.run(regex, string, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> hd()
end
