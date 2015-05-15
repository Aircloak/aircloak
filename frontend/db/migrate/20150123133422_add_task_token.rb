class AddTaskToken < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        add_column :tasks, "token", :text
        add_index :tasks, :token, unique: true

        Task.all.each do |task|
          task.generate_token
          raise "Could not change task token for task #{task.id}" unless task.save
        end

        change_column_null :tasks, "token", false
      end

      dir.down do
        remove_column :tasks, "token"
      end
    end
  end
end
