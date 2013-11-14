class RemoveTpmFromCluster < ActiveRecord::Migration
  def change
    remove_column :clusters, :tpm, :boolean
  end
end
