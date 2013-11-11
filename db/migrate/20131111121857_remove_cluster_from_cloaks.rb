class RemoveClusterFromCloaks < ActiveRecord::Migration
  def change
    remove_column :cloaks, :cluster, :belongs_to
  end
end
