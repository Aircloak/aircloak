class CreateRaTaskCodes < ActiveRecord::Migration
  def change
    create_table :ra_task_codes do |t|
      t.text :prefetch
      t.text :code

      t.boolean :trustworthy, default: false

      t.timestamps
    end
  end
end
