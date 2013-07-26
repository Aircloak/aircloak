class ChangePropertiesResultsTableName < ActiveRecord::Migration
  def change
     rename_table :properties_results, :property_results
  end
end
