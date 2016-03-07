class CreateTask < ActiveRecord::Migration
  def change
    create_table :tasks do |t|
      t.boolean :ready, default: false

      t.boolean :update_task, default: false
      t.string :payload_identifier

      t.boolean :system_task, default: false
      t.boolean :mutator, defaut: false

      t.binary :packaged_data
      t.string :main_package

      t.timestamps
    end
  end
end
