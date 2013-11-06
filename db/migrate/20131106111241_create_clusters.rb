class CreateClusters < ActiveRecord::Migration
  def change
    create_table :clusters do |t|
      t.string :name
      t.boolean :tpm, default: true

      t.timestamps
    end
  end
end
