class CreatePropertyResultCounts < ActiveRecord::Migration
  def change
    create_table :property_result_counts do |t|
      t.references :property_result
      t.integer :count
      t.timestamps
    end
  end
end
