class RemoveBuildOutputFromBuildVersions < ActiveRecord::Migration
  def change
    remove_column :build_versions, :log_output, :string
    remove_column :build_versions, :build_complete, :string
    remove_column :build_versions, :build_success, :string
  end
end
