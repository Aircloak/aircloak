class AddClientFileIdToClientFileEvent < ActiveRecord::Migration
  def change
    add_reference :client_file_events, :client_file, index: true
    ClientFileEvent.all.each do |event|
      event.client_file = event.client_file_version.client_file
      event.save
    end
  end
end
