class DropQueryFiles < ActiveRecord::Migration
  def change
    drop_table :query_files
  end
end
