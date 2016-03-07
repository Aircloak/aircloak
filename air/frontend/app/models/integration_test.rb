class IntegrationTest < ActiveRecord::Base
  has_many :integration_test_results

  def self.add_result_for_test identifier, result
    test = IntegrationTest.find_by_identifier identifier
    test.integration_test_results.create(result: result)
  end

  def results
    integration_test_results.map(&:json_result).to_json
  end
end
