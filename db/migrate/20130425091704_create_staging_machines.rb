class CreateStagingMachines < ActiveRecord::Migration
  def change
    create_table :staging_machines do |t|
      t.string :name
      t.string :description

      t.timestamps
    end
  end
end
