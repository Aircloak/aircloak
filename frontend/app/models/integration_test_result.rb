class IntegrationTestResult < ActiveRecord::Base
  def json_result
    result = JSON.parse(self.result)
    result[:created_at] = created_at.to_i
    result
  end
end
