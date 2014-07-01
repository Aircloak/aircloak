class RemoveInquirerDbPermission < ActiveRecord::Migration
  def change
    Permission.where(name: "inquirer").destroy_all
  end
end
