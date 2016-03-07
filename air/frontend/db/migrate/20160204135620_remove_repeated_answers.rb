class RemoveRepeatedAnswers < ActiveRecord::Migration
  def change
    [
      :repeated_answers,
      :ra_library_code_ra_task_codes,
      :ra_library_codes,
      :ra_task_code_clusters,
      :ra_task_codes
    ].each do |table_name|
      # We have to do some conditional magic here.
      # The tables will exist on our own local installations,
      # but not for new migrations from scratch.
      # Therefore we only remove them if they do exist
      drop_if_exists table_name
    end
  end

  def drop_if_exists table_name
    drop_table table_name if ActiveRecord::Base.connection.table_exists? table_name
  end
end
