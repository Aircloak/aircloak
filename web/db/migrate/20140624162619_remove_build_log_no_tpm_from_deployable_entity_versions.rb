class RemoveBuildLogNoTpmFromDeployableEntityVersions < ActiveRecord::Migration
  def change
    remove_column :deployable_entity_versions, :build_log_no_tpm, :text
  end
end
