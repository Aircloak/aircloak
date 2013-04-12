class AddHumanNameToClientFileType < ActiveRecord::Migration
  def change
    add_column :client_file_types, :human_name, :string
  end
end
