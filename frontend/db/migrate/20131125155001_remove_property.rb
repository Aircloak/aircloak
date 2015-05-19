class RemoveProperty < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        drop_table :properties
      end

      dir.down do
        raise ActiveRecord::IrreversibleMigration
      end
    end
  end
end
