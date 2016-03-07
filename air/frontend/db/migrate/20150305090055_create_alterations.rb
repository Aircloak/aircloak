class CreateAlterations < ActiveRecord::Migration
  def change
    create_table :alterations do |t|
      t.string :target_type
      t.integer :target_id
      t.references :user, index: true
      t.text :description

      t.timestamps
    end
  end
end
