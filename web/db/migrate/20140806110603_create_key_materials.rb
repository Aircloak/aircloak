class CreateKeyMaterials < ActiveRecord::Migration
  def change
    create_table :key_materials do |t|
      t.references :analyst
      t.string :description
      t.text :certificate
      t.text :key
      t.text :pkcs12

      t.timestamps
    end
  end
end
