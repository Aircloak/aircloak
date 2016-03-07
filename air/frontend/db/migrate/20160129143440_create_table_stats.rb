class CreateTableStats < ActiveRecord::Migration
  def change
    create_table :user_table_stats do |t|
      t.integer :user_table_id
      t.integer :num_users
      t.integer :num_rows
      t.timestamps
    end
  end
end
