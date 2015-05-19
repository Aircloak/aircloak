class CreateBucket < ActiveRecord::Migration
  def change
    create_table :buckets do |t|
      t.belongs_to :result
      t.string :label
      t.string :str_answer
      t.integer :range_min
      t.integer :range_max
      t.integer :joiners
      t.integer :leavers
      t.integer :accumulated_count
    end
  end
end
