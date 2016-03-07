class ExtendTasksForStreaming < ActiveRecord::Migration
  def change
    add_column :tasks, :task_type, :int, null: false, default: 1
    add_column :tasks, :report_interval, :int
    add_column :tasks, :user_expire_interval, :int
  end
end
