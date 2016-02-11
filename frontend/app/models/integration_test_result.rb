class IntegrationTestResult < ActiveRecord::Base
  def json_result
    return @result if @result

    # Prep the results for display
    @result = JSON.parse(self.result)
    @result["created_at"] = created_at.to_i

    # Change the timing label categories
    # into something human readable
    fixed_timings = []
    @result["timings"].each do |item|
      fixed_timing = {}
      item.each_pair do |key, value|
        fixed_timing[key.humanize] = value
      end
      fixed_timings << fixed_timing
    end
    @result["timings"] = fixed_timings
    @result
  end

  def success
    json_result["success"]
  end

  def cloaks
    return "" if json_result["cloaks"].nil?
    json_result["cloaks"].join(", ")
  end

  def last_stage
    return "" if json_result["last_stage"].nil?
    json_result["last_stage"].humanize
  end

  def description
    return "" if json_result["result_description"].nil?
    json_result["result_description"]
  end

  def test_duration
    total_time_s = 0
    json_result["timings"].first.each_pair do |test, time|
      total_time_s += time
    end
    minutes = total_time_s / 60
    seconds = total_time_s % 60
    sprintf("%d:%02d", minutes, seconds)
  end
end
