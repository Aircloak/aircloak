class CreateProperties < ActiveRecord::Migration
  def change
    create_table :properties do |t|
      t.references :query, index: true
      t.string :property

      t.timestamps
    end
  end
end
