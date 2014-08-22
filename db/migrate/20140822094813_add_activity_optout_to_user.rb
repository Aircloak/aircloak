class AddActivityOptoutToUser < ActiveRecord::Migration
  def change
    add_column :users, :activity_monitoring_opt_out, :boolean, default: false
  end
end
