class RemoveTpmEnvAndNoTpmEnvFromDeployableEntities < ActiveRecord::Migration
  def change
    remove_column :deployable_entities, :tpm_env, :string
    remove_column :deployable_entities, :no_tpm_env, :string
  end
end
