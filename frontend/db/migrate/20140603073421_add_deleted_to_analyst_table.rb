class AddDeletedToAnalystTable < ActiveRecord::Migration
  def change
    add_column :analyst_tables, :deleted, :boolean, default: false
  end
end
