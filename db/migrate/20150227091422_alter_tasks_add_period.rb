class AlterTasksAddPeriod < ActiveRecord::Migration
  def change
    add_column :tasks, :period, :text
  end
end
