class AddInquirerRole < ActiveRecord::Migration
  def change
    Permission.create(name: "inquirer", description: "user who can act on behalf of an analyst")
  end
end
