class RemovePartOfRingFromCloak < ActiveRecord::Migration
  def change
    remove_column :cloaks, :part_of_ring, :boolean, default: false
  end
end
