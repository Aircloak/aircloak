class CreateQueries < ActiveRecord::Migration
  def change
    create_table :queries do |t|
      t.string :name
      t.references :index

      t.boolean :update_query, :default => false
      t.string :identifier

      t.boolean :system_query, :default => false
      t.boolean :mutator, :default => false

      t.timestamps
    end
  end
end
