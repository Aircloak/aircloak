class RemoveRepeatedAnswers < ActiveRecord::Migration
  def change
    drop_table :repeated_answers
    drop_table :ra_library_code_ra_task_codes
    drop_table :ra_library_codes
    drop_table :ra_task_code_clusters
    drop_table :ra_task_codes
  end
end
