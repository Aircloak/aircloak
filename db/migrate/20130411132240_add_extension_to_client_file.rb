class AddExtensionToClientFile < ActiveRecord::Migration
  def change
    add_column :client_files, :extension, :string
  end
end
