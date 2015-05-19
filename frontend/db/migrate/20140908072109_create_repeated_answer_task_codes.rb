class CreateRepeatedAnswerTaskCodes < ActiveRecord::Migration
  def change
    create_table :repeated_answer_task_codes do |t|
      t.belongs_to :repeated_answer, index: true

      t.string :code

      t.timestamps
    end
  end
end
