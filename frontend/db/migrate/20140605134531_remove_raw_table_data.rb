class RemoveRawTableData < ActiveRecord::Migration
  def change
    remove_column :analyst_tables, :raw_table_data, :text
  end
end
