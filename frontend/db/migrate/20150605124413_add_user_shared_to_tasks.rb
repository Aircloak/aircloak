class AddUserSharedToTasks < ActiveRecord::Migration
  def change
    add_column :tasks, :shared, :boolean, default: true, null: false
    add_reference :tasks, :user, index: true, default: 0, null: false
  end
end
