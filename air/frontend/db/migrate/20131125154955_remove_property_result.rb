class RemovePropertyResult < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        drop_table :property_results
      end

      dir.down do
        raise ActiveRecord::IrreversibleMigration
      end
    end
  end
end
