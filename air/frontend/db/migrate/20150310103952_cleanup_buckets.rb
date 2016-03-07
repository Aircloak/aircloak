class CleanupBuckets < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        rename_column :buckets, :str_answer, :value
        rename_column :buckets, :accumulated_count, :count
      end
      dir.down do
        rename_column :buckets, :value, :str_answer
        rename_column :buckets, :count, :accumulated_count
      end
    end
  end
end
