class AddPurgedToTask < ActiveRecord::Migration
  def change
    add_column :tasks, :purged, :boolean, :default => false, :null => false
  end
end
