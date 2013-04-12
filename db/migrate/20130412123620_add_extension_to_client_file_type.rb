class AddExtensionToClientFileType < ActiveRecord::Migration
  def change
    add_column :client_file_types, :extension, :string
  end
end
