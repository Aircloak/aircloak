class CreateBuildVersions < ActiveRecord::Migration
  def change
    create_table :build_versions do |t|
      t.references :deployable_entity_version, index: true
      t.references :build, index: true
      t.string :log_output
      t.boolean :build_complete
      t.boolean :build_success

      t.timestamps
    end
  end
end
