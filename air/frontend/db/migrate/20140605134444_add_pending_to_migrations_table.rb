class AddPendingToMigrationsTable < ActiveRecord::Migration
  def change
    add_column :analyst_table_migrations, :migrated, :boolean, default: false
    # The state of the table at this given version
    # It allows us to revert back to a previous table state
    # should later table migrations have failed.
    add_column :analyst_table_migrations, :table_json, :text
  end
end
