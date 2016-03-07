class AddFullIntegrationTest < ActiveRecord::Migration
  def change
    integration_test = IntegrationTest.new(
        name: "Full end-to-end test",
        identifier: "full_cluster_integration_test")
    unless integration_test.save
      raise integration_test.errors
    end
  end
end
