class CreateTestItemVms < ActiveRecord::Migration
  def change
    create_table :test_item_vms do |t|
      t.belongs_to :test_item, index: true
      t.belongs_to :test_vm, index: true

      t.integer :cpus
      t.integer :memory_size
      t.integer :memory_usage
      t.integer :disk_usage

      t.timestamps
    end
  end
end
