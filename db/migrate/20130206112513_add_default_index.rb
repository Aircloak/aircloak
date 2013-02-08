class AddDefaultIndex < ActiveRecord::Migration
  def change
    Index.create(human_name: "All users", name: "all_users", system_index: true)
  end
end
