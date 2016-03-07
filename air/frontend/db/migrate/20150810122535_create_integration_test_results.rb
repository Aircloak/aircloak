class CreateIntegrationTestResults < ActiveRecord::Migration
  def change
    create_table :integration_test_results do |t|
      t.belongs_to :integration_test
      t.text :result

      t.timestamps
    end

    add_index :integration_test_results, :integration_test_id
  end
end
