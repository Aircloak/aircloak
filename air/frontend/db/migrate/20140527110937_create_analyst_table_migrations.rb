class CreateAnalystTableMigrations < ActiveRecord::Migration
  def change
    create_table :analyst_table_migrations do |t|
      t.integer :version
      t.text :migration
      t.references :analyst_table, index: true

      t.timestamps
    end
  end
end
