class RemoveTpmFromBuilds < ActiveRecord::Migration
  def change
    remove_column :builds, :tpm, :boolean
  end
end
