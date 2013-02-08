class CreateTempQueryFiles < ActiveRecord::Migration
  def change
    create_table :temp_query_files do |t|
      t.binary :data

      t.timestamps
    end
  end
end
