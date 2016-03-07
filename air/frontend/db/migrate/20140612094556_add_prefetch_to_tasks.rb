class AddPrefetchToTasks < ActiveRecord::Migration
  def change
    reversible do |dir|
      # Up to now, code was string (meaning it had default lenght of 255).
      # We're changing this to text, to allow longer contents.
      dir.up do
        # Using change_column left the length limit on the new column, so
        # I'm recreating the column.
        remove_column :tasks, :code
        add_column :tasks, :code, :text
      end

      dir.down do
        remove_column :tasks, :code
        add_column :tasks, :code, :string
      end
    end

    add_column :tasks, :prefetch, :text
  end
end