class CreateAnalystTables < ActiveRecord::Migration
  def change
    create_table :analyst_tables do |t|
      t.string :table_name
      t.references :cluster, index: true
      t.text :raw_table_data

      t.timestamps
    end
  end
end
