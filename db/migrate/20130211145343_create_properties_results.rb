class CreatePropertiesResults < ActiveRecord::Migration
  def change
    create_table :property_results do |t|
      t.string :bucket
      t.boolean :numeric, :default => false
      t.string :str_value
      t.integer :long_value
      t.references :query, index: true

      # DELETED COUNT
      
      t.timestamps
    end
    add_index :property_results, :bucket
    add_index :property_results, :str_value
    add_index :property_results, :long_value
    add_index :property_results, :numeric
  end
end
