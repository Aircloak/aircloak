class AddDeletedToTasks < ActiveRecord::Migration
  def change
    add_column :tasks, :deleted, :boolean, :default => false, :null => false
  end
end
