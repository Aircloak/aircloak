class CreateAnalystsClusters < ActiveRecord::Migration
  def change
    create_table :analysts_clusters, :id => false do |t|
      t.references :cluster, index: true
      t.references :analyst, index: true

      t.timestamps
    end

    add_index :analysts_clusters, [:cluster_id, :analyst_id], :unique => true
  end
end
