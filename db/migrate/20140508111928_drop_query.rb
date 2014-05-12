class DropQuery < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        drop_table :queries
      end
      dir.down do
        create_table :queries do |t|
          t.string :name
          t.belongs_to :task
          t.belongs_to :cluster

          t.timestamps
        end
      end
    end
  end
end
