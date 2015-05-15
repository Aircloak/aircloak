class AddDefaultPermissions < ActiveRecord::Migration
  def change
    permissions = [
      ["admin", "all seeing, all controlling"]
    ]
    permissions.each do |name, description|
      Permission.create(name: name, description: description)
    end
  end
end
