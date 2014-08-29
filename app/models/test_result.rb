class TestResult < ActiveRecord::Base
  validates_presence_of :testtime, :test_server_version, :duration, :benchmark_duration, :benchmark_coverage,
      :benchmark_memory_usage
  validates_uniqueness_of :testtime
  has_many :test_vms
  has_many :test_items

  def testtime_in_UTC
    Time.at(testtime).utc.strftime('%Y-%m-%d %H:%M')
  end

  def success
    benchmark_success && test_vms.all? { |vm| vm.success } && test_items.all? { |item| item.success }
  end

  def display_test_server_version
    return test_server_version[0..9]
  end
end
