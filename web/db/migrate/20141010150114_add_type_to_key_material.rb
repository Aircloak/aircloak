class AddTypeToKeyMaterial < ActiveRecord::Migration
  def change
    add_column :key_materials, :key_type, :string
    KeyMaterial.all.each do |key|
      key.key_type = "data_upload_all"
      key.save
    end
  end
end
