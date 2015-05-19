class CreateAnalysts < ActiveRecord::Migration
  def change
    create_table :analysts do |t|
      t.string :name
      t.text :key
      t.text :certificate

      t.timestamps
    end
  end
end
