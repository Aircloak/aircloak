class AddCreatedAtToResults < ActiveRecord::Migration
  def change
    add_column :results, :created_at, :datetime
  end
end
