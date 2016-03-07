class AddRequiresVerificationToClientFile < ActiveRecord::Migration
  def change
    add_column :client_files, :requires_verifications, :boolean, default: true
  end
end
