class ShortenTaskTokens < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        # Drop column, then recreate it and generate tokens.
        remove_column :tasks, "token"

        add_column :tasks, "token", :text
        add_index :tasks, :token, unique: true

        Task.all.each do |task|
          task.generate_token
          raise "Could not change task token for task #{task.id}" unless task.save
        end

        change_column_null :tasks, "token", false
      end

      dir.down do
        # Don't need to do anything since column is already in place
      end
    end
  end
end
