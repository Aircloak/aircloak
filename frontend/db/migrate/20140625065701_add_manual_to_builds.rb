class AddManualToBuilds < ActiveRecord::Migration
  def change
    add_column :builds, :manual, :boolean
  end
end
