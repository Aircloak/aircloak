class AddBuildLogFromDeployableEntityVersions < ActiveRecord::Migration
  def change
    add_column :deployable_entity_versions, :build_log, :text
  end
end
