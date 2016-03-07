class CreateQueryIndices < ActiveRecord::Migration
  def change
    create_table :query_indices do |t|
      t.references :query, index: true
      t.references :user, index: true
      t.string :name

      t.timestamps
    end
  end
end
