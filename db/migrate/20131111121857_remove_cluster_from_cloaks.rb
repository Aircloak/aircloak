class RemoveClusterFromCloaks < ActiveRecord::Migration
  def change
    remove_column :cloaks, :cluster_id, :belongs_to
  end
end
