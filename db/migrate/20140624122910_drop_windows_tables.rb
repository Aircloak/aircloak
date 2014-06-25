class DropWindowsTables < ActiveRecord::Migration
  def change
    drop_table :client_binaries
    drop_table :client_file_events
    drop_table :client_file_types
    drop_table :client_file_versions
    drop_table :client_files
    drop_table :command_file_versions
    drop_table :commands
    drop_table :deployment_groups
    drop_table :staging_machines
  end
end
