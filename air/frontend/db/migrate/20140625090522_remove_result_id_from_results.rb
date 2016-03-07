class RemoveResultIdFromResults < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        remove_column :results, :result_id
      end
      dir.down do
        add_column :results, :result_id, :integer
      end
    end
  end
end
