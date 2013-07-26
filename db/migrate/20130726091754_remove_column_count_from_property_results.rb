class RemoveColumnCountFromPropertyResults < ActiveRecord::Migration
  def change
    remove_column :property_results, :count
  end
end
