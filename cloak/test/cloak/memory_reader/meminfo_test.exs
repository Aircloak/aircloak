defmodule Cloak.MemoryReader.MemInfoTest do
  use ExUnit.Case, async: true

  alias Cloak.MemoryReader.MemInfo

  test "parses meminfo data" do
    assert %MemInfo{total_memory: 2047144, free_memory: 1768660} == MemInfo.parse(meminfo())
  end

  def meminfo() do
    """
MemTotal:        2047144 kB
MemFree:         1768660 kB
MemAvailable:    1716936 kB
Buffers:           15520 kB
Cached:           165268 kB
SwapCached:            0 kB
Active:           125392 kB
Inactive:         105248 kB
Active(anon):     104240 kB
Inactive(anon):    98532 kB
Active(file):      21152 kB
Inactive(file):     6716 kB
Unevictable:           0 kB
Mlocked:               0 kB
SwapTotal:       4090876 kB
SwapFree:        4090876 kB
Dirty:                 0 kB
Writeback:             0 kB
AnonPages:         50056 kB
Mapped:            51868 kB
Shmem:            152800 kB
Slab:              33040 kB
SReclaimable:      19228 kB
SUnreclaim:        13812 kB
KernelStack:        3376 kB
PageTables:         1196 kB
NFS_Unstable:          0 kB
Bounce:                0 kB
WritebackTmp:          0 kB
CommitLimit:     5114448 kB
Committed_AS:     697600 kB
VmallocTotal:   34359738367 kB
VmallocUsed:           0 kB
VmallocChunk:          0 kB
AnonHugePages:         0 kB
ShmemHugePages:        0 kB
ShmemPmdMapped:        0 kB
HugePages_Total:       0
HugePages_Free:        0
HugePages_Rsvd:        0
HugePages_Surp:        0
Hugepagesize:       2048 kB
DirectMap4k:       18432 kB
DirectMap2M:     2078720 kB
DirectMap1G:     3145728 kB
    """
  end
end
