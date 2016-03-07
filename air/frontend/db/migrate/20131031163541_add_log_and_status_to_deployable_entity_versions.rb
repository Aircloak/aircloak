class AddLogAndStatusToDeployableEntityVersions < ActiveRecord::Migration
  def change
    add_column :deployable_entity_versions, :build_completed, :boolean
    add_column :deployable_entity_versions, :build_success, :boolean
    add_column :deployable_entity_versions, :build_log_tpm, :string
    add_column :deployable_entity_versions, :build_log_no_tpm, :string
  end
end
