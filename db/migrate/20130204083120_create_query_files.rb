class CreateQueryFiles < ActiveRecord::Migration
  def change
    create_table :query_files do |t|
      t.string :sha
      t.string :name
      t.references :query
      t.binary :data
      t.boolean :query_interface
      t.boolean :index_ops
      t.string :package

      t.timestamps
    end
  end
end
