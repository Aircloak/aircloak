class TestResultsChangeToText < ActiveRecord::Migration
  def change
    change_column :test_vms, :log, :text
    change_column :test_items, :log, :text
  end
end
