class CreateLookupTables < ActiveRecord::Migration
  def change
    create_table :lookup_tables do |t|
      t.string :table_name, null: false
      t.boolean :deleted, default: false, null: false
      t.references :cluster, index: true, null: false
      t.references :analyst, index: true, null: false
      t.timestamps
    end
  end
end
