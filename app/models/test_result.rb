class TestResult < ActiveRecord::Base
  validates_presence_of :testtime, :duration, :benchmark_duration, :benchmark_coverage,
      :benchmark_memory_usage, :benchmark_cloak_core_time, :benchmark_postgresql_time, :benchmark_sandbox_time
  validates_uniqueness_of :testtime
  has_many :test_vms
  has_many :test_items
end
