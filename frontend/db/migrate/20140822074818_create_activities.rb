class CreateActivities < ActiveRecord::Migration
  def change
    create_table :activities do |t|
      t.references :user, index: true
      t.string :description
      t.string :path
      t.boolean :success

      t.timestamps
    end
  end
end
