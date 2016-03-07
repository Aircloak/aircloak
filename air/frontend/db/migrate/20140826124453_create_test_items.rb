class CreateTestItems < ActiveRecord::Migration
  def change
    create_table :test_items do |t|
      t.belongs_to :test_result, index: true

      t.string :name
      t.boolean :success
      t.integer :duration
      t.string :log

      t.timestamps
    end
  end
end
