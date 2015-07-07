class CreateTestVms < ActiveRecord::Migration
  def change
    create_table :test_vms do |t|
      t.belongs_to :test_result, index: true

      t.string :name
      t.boolean :success
      t.integer :duration
      t.integer :disk_size
      t.integer :disk_usage
      t.string :log

      t.timestamps
    end
  end
end
