class AddUserToKeyMaterials < ActiveRecord::Migration
  def change
    add_reference :key_materials, :user, index: true
  end
end
