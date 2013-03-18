class CreateClientBinaries < ActiveRecord::Migration
  def change
    create_table :client_binaries do |t|
      t.boolean :updater, default: false
      t.integer :size
      t.string :sha1
      t.binary :data

      t.timestamps
    end
  end
end
