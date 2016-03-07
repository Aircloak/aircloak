class AddCodeTimestampToTask < ActiveRecord::Migration
  def change
    add_column :tasks, :code_timestamp, :datetime, :default => Time.now
  end
end
