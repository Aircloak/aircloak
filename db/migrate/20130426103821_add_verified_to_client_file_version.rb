class AddVerifiedToClientFileVersion < ActiveRecord::Migration
  def change
    add_column :client_file_versions, :verified, :boolean, default: false
  end
end
