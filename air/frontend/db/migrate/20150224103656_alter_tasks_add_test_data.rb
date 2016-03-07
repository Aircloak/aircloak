class AlterTasksAddTestData < ActiveRecord::Migration
  def change
    add_column :tasks, :test_data, :text
  end
end
