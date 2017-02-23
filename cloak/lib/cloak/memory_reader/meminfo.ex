defmodule Cloak.MemoryReader.MemInfo do
  @moduledoc """
  Allows parsing the information returned when reading /proc/meminfo
  on a linux machine.

  All memory values returned are in kB.
  """

  @type t :: %__MODULE__{
    total_memory: non_neg_integer,
    free_memory: non_neg_integer,
  }

  defstruct total_memory: 0, free_memory: 0
end
