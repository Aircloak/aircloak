class CreateRaTaskCodeClusters < ActiveRecord::Migration
  def change
    create_table :ra_task_code_clusters do |t|
      t.belongs_to :ra_task_code, index: true
      t.belongs_to :cluster, index: true

      t.timestamps
    end
  end
end
