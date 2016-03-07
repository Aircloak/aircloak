class CreateDeployableEntityVersions < ActiveRecord::Migration
  def change
    create_table :deployable_entity_versions do |t|
      t.references :deployable_entity, index: true
      t.string :commit_id
      t.string :url
      t.string :message
      t.string :author

      t.timestamps
    end
  end
end
