class PropertiesController < ApplicationController
  def show
    @property = Property.find(params[:id])
    sql = <<-SQL
      SELECT property_results.*, 
        (SELECT property_result_counts.joiners AS joiners, property_result_counts.leavers AS leavers
         FROM property_result_counts
         WHERE property_result_counts.property_result_id = property_results.id
         ORDER BY property_result_counts.id ASC)
      FROM property_results
      WHERE property_results.property_id = #{@property.id}
      ORDER BY property_results.task_id, property_results.index,
               property_results.has_range, property_results.range_min, property_results.range_max ASC
    SQL
    @results = PropertyResult.connection.select_all sql
  end
end
