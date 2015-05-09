class CreateIndices < ActiveRecord::Migration
  def change
    create_table :indices do |t|
      t.string :name, :default => ""
      t.string :human_name, :default => ""
      # A system query is one that can be seen by any analyst.
      t.boolean :system_index, :default => false

      t.timestamps
    end
  end
end
