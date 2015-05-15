class AddPendingDeleteToAnalystTables < ActiveRecord::Migration
  def change
    add_column :analyst_tables, :pending_delete, :boolean, default: false
  end
end
