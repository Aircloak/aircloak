class CreateClusters < ActiveRecord::Migration
  def change
    create_table :clusters do |t|
      t.string :name
      t.boolean :tpm, default: true
      t.belongs_to :build, index: true

      t.timestamps
    end
  end
end
