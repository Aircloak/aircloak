class AddExtensionToClientFile < ActiveRecord::Migration
  def change
    add_column :client_file_types, :extension, :string

    ClientFileType.all.each do |cft|
      cft.extension = cft.client_files.first.extension
      cft.save
    end

    remove_column :client_files, :extension
  end
end
