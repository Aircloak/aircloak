class CleanupQuery < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        add_belongs_to :queries, :task, index: true
        add_belongs_to :queries, :cluster, index: true
        remove_column :queries, :index_id
        remove_column :queries, :update_query
        remove_column :queries, :identifier
        remove_column :queries, :system_query
        remove_column :queries, :mutator
        remove_column :queries, :main_package
        remove_column :queries, :packaged_data
        remove_column :queries, :manages_indices

        drop_table :indices
        drop_table :query_indices
      end

      dir.down do
        raise ActiveRecord::IrreversibleMigration
      end
    end
  end
end
