class CreateExceptionResults < ActiveRecord::Migration
  def change
    create_table :exception_results do |t|
      t.references :query, index: true
      t.string :stack, array: true
      t.integer :count

      t.timestamps
    end
    add_index :exception_results, :count
  end
end
