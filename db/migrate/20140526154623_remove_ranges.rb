class RemoveRanges < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        remove_column :buckets, :range_min
        remove_column :buckets, :range_max
      end
      dir.down do
        add_column :buckets, :range_min, :integer
        add_column :buckets, :range_max, :integer
      end
    end
  end
end
