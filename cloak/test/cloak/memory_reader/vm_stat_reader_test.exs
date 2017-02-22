defmodule Cloak.MemoryReader.VMStatReaderTest do
  use ExUnit.Case, async: true

  alias Cloak.MemoryReader.{MemInfo, VMStatReader}

  test "parses vm_stat data" do
    assert %MemInfo{total_memory: 7293832, free_memory: 2783756} == VMStatReader.parse(meminfo())
  end

  def meminfo() do
    """
Mach Virtual Memory Statistics: (page size of 4096 bytes)
Pages free:                               20009.
Pages active:                            731201.
Pages inactive:                          675930.
Pages speculative:                        35248.
Pages throttled:                              0.
Pages wired down:                        396318.
Pages purgeable:                         108545.
"Translation faults":                 305097221.
Pages copy-on-write:                    6269570.
Pages zero filled:                     84506303.
Pages reactivated:                     13519765.
Pages purged:                           2524622.
File-backed pages:                       276967.
Anonymous pages:                        1165412.
Pages stored in compressor:              857757.
Pages occupied by compressor:            237907.
Decompressions:                        10200623.
Compressions:                          16305807.
Pageins:                                7331331.
Pageouts:                                211761.
Swapins:                                 931065.
Swapouts:                               1195265.
    """
  end
end
