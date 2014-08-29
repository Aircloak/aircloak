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

  def success_class
    return "" if success
    return "error"
  end

  def display_test_server_version
    return test_server_version[0..9]
  end

  def benchmark_duration_class old_result
    choose_class "error", "success", old_result, lambda {|r| r.benchmark_duration}
  end

  def benchmark_coverage_class old_result
    choose_class "success", "error", old_result, lambda {|r| r.benchmark_coverage}
  end

  def benchmark_memory_usage_class old_result
    choose_class "error", "success", old_result, lambda {|r| r.benchmark_memory_usage}
  end

private

  def choose_class class_greater, class_less, old_result, get_flag
    return "" unless old_result
    return class_greater if get_flag.call(self) > get_flag.call(old_result)
    return class_less if get_flag.call(self) < get_flag.call(old_result)
    return ""
  end
end
