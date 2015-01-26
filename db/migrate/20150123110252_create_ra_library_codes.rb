class CreateRaLibraryCodes < ActiveRecord::Migration
  def change
    create_table :ra_library_codes do |t|
      t.text :code, index: true, unique: true

      t.timestamps
    end
  end
end
