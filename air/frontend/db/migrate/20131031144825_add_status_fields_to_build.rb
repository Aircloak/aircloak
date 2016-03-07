class AddStatusFieldsToBuild < ActiveRecord::Migration
  def change
    add_column :builds, :build_completed, :boolean
    add_column :builds, :build_success, :boolean
  end
end
