class AddGroupToCommands < ActiveRecord::Migration
  def change
    add_reference :commands, :deployment_group, index: true
  end
end
