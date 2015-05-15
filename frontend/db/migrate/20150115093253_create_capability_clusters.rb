class CreateCapabilityClusters < ActiveRecord::Migration
  def change
    create_table :capability_clusters do |t|
      t.belongs_to :cluster
      t.belongs_to :capability

      t.timestamps
    end
  end
end
