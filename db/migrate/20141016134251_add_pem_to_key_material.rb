class AddPemToKeyMaterial < ActiveRecord::Migration
  def change
    add_column :key_materials, :pem, :text
  end
end
