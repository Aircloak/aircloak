class AlterTableStatsAddStatus < ActiveRecord::Migration
  def change
    add_column :user_table_stats, :success, :boolean, default: true
  end
end
