class CreateTestResults < ActiveRecord::Migration
  def change
    create_table :test_results do |t|
      t.integer :testtime
      t.string :test_server_version
      t.integer :duration

      t.boolean :benchmark_success
      t.integer :benchmark_duration
      t.float :benchmark_coverage
      t.integer :benchmark_memory_usage

      t.timestamps
    end
  end
end
