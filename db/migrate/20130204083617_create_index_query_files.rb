class CreateIndexQueryFiles < ActiveRecord::Migration
  def change
    create_table :index_query_files do |t|
      t.references :index, :query_file

      t.timestamps
    end
  end
end
