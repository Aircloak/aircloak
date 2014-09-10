class AddPrefetchToRepeatedAnswerTaskCode < ActiveRecord::Migration
  def change
    add_column :repeated_answer_task_codes, :prefetch, :string
  end
end
