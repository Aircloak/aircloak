class CreateCapabilities < ActiveRecord::Migration
  def change
    create_table :capabilities do |t|
      t.string :identifier
      t.string :name
      t.text :description

      t.timestamps
    end
  end
end
