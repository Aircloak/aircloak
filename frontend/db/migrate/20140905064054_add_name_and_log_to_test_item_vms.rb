class AddNameAndLogToTestItemVms < ActiveRecord::Migration
  def change
    add_column :test_item_vms, :name, :string
    add_column :test_item_vms, :log, :text
  end
end
