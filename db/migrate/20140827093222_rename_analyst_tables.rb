class RenameAnalystTables < ActiveRecord::Migration
  def change
    rename_column :analyst_table_migrations, :analyst_table_id, :user_table_id
    rename_table :analyst_table_migrations, :user_table_migrations
    rename_table :analyst_tables, :user_tables
  end
end
