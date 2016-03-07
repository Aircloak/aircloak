class AddClientFileIdToClientFileEvent < ActiveRecord::Migration
  def change
    add_reference :client_file_events, :client_file, index: true
  end
end
