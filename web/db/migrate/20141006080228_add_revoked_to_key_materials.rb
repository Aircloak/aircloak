class AddRevokedToKeyMaterials < ActiveRecord::Migration
  def change
    add_column :key_materials, :revoked, :boolean, :default => false
  end
end
