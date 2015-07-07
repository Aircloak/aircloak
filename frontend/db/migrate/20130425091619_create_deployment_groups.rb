class CreateDeploymentGroups < ActiveRecord::Migration
  def change
    create_table :deployment_groups do |t|
      t.string :identifier
      t.string :name
      t.boolean :verified_only, :default => true
      t.boolean :autoupdate, :default => false

      t.timestamps
    end
  end
end
