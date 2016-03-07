class CreateCloaks < ActiveRecord::Migration
  def change
    create_table :cloaks do |t|
      t.string :name
      t.string :ip
      t.boolean :part_of_ring, default: false

      t.timestamps
    end
  end
end
