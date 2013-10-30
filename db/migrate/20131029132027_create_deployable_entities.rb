class CreateDeployableEntities < ActiveRecord::Migration
  def change
    create_table :deployable_entities do |t|
      t.string :repo
      t.string :description
      t.string :tpm_env
      t.string :no_tpm_env

      t.timestamps
    end
  end
end
