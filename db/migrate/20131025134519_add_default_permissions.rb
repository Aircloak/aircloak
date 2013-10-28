class AddDefaultPermissions < ActiveRecord::Migration
  def change
    permissions = [
      ["admin", "all seeing, all controlling"],
      ["ops", "staging and deployment"],
      ["client_manager", "manages windows client"],
      ["deploy_manager", "manages deployment of windows client versions"]
    ]
    permissions.each do |name, description|
      Permission.create(name: name, description: description)
    end
  end
end
