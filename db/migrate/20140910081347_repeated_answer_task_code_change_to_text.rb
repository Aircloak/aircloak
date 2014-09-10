class RepeatedAnswerTaskCodeChangeToText < ActiveRecord::Migration
  def change
    change_column :repeated_answer_task_codes, :prefetch, :text
    change_column :repeated_answer_task_codes, :code, :text
  end
end
