class CreateClientFileTypes < ActiveRecord::Migration
  def change
    create_table :client_file_types do |t|
      t.string :name
      t.string :extension
      t.string :human_name

      t.timestamps
    end
  end
end
