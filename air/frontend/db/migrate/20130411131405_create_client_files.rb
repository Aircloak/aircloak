class CreateClientFiles < ActiveRecord::Migration
  def change
    create_table :client_files do |t|
      t.string :name
      t.string :local_name
      t.references :client_file_type

      t.timestamps
    end
  end
end
