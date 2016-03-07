class AddOneOfFlagToTasks < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        add_column :tasks, :one_off, :boolean, default: false
      end

      dir.down do
        remove_column :tasks, :one_off
      end
    end
  end
end
