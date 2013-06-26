class AddManagesIndexFieldToQueries < ActiveRecord::Migration
  def change
    add_column :queries, :manages_indices, :boolean, default: false
  end
end
