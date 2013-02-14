class CreatePropertiesResults < ActiveRecord::Migration
  def change
    create_table :properties_results do |t|
      t.string :bucket
      t.boolean :numeric, :default => false
      t.string :str_value
      t.integer :long_value
      t.integer :count
      t.references :query, index: true

      t.timestamps
    end
    add_index :properties_results, :bucket
    add_index :properties_results, :str_value
    add_index :properties_results, :long_value
    add_index :properties_results, :numeric
    add_index :properties_results, :count
  end
end
