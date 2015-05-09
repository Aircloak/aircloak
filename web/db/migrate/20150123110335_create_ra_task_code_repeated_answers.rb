class CreateRaTaskCodeRepeatedAnswers < ActiveRecord::Migration
  def change
    create_table :ra_task_code_repeated_answers do |t|
      t.belongs_to :ra_task_code, index: true
      t.belongs_to :repeated_answer, index: true

      t.timestamps
    end
  end
end
