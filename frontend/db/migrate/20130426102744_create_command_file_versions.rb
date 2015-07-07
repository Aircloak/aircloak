class CreateCommandFileVersions < ActiveRecord::Migration
  def change
    create_table :command_file_versions do |t|
      t.references :command, index: true
      t.references :client_file_version, index: true

      t.timestamps
    end
  end
end
