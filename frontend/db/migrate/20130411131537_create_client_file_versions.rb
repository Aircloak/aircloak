class CreateClientFileVersions < ActiveRecord::Migration
  def change
    create_table :client_file_versions do |t|
      t.binary :data
      t.string :sha1
      t.integer :size
      t.integer :times_downloaded
      t.references :client_file
      t.boolean :verified, default: false

      t.timestamps
    end
  end
end
