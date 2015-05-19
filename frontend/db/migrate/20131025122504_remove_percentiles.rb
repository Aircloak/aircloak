class RemovePercentiles < ActiveRecord::Migration
  def change
    [:percentiles, :percentile_results].each do |table|
      drop_table table if table_exists? table
    end
  end
end
