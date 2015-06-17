class AddClusterManagerPermission < ActiveRecord::Migration
  def change
    permission = Permission.create(name: "cluster manager",
        description: "can manage analyst's clusters (generate keys, setup tables, upload data, etc.)")
    permission.users = User.all
    permission.save!
  end
end
