class AlterAnalystId < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        remove_column :exception_results, :analyst_id
        add_column :exception_results, :analyst_id, :integer
      end

      dir.down do
        remove_column :exception_results, :analyst_id
        add_column :exception_results, :analyst_id, :string
      end
    end
  end
end
