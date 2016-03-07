class AddStateToClusterCloak < ActiveRecord::Migration
  def change
    add_column :cluster_cloaks, :raw_state, :integer, :default => 1
  end
end
