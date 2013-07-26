class PropertiesController < ApplicationController
  def show
    @property = Property.find(params[:id])
    sql = <<-SQL
      SELECT property_results.*, 
        (SELECT property_result_counts.count FROM property_result_counts
         WHERE property_result_counts.property_result_id = property_results.id
         ORDER BY property_result_counts.id DESC
         LIMIT 1) as count
      FROM property_results
      WHERE property_results.property_id = #{@property.id}
      ORDER BY count DESC
    SQL
    results = PropertyResult.connection.select_all sql
    @results = results.to_a.sort do |a,b|
      b["count"].to_i <=> a["count"].to_i
    end
  end
end
