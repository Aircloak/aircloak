class AlterBucketStringToText < ActiveRecord::Migration
  reversible do |dir|
    dir.up do
      change_column :buckets, :label, :text
      change_column :buckets, :str_answer, :text
    end

    dir.down do
      change_column :buckets, :label, :string
      change_column :buckets, :str_answer, :string
    end
  end
end
