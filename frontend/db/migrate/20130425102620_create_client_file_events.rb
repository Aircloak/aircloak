class CreateClientFileEvents < ActiveRecord::Migration
  def change
    create_table :client_file_events do |t|
      t.boolean :positive, :default => true
      t.string :description
      t.string :event
      t.references :client_file_version
      t.references :staging_machine

      t.timestamps
    end
  end
end
