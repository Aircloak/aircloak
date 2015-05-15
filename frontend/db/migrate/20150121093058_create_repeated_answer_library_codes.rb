class CreateRepeatedAnswerLibraryCodes < ActiveRecord::Migration
  def change
    create_table :repeated_answer_library_codes do |t|
      t.belongs_to :repeated_answer_task_code
      t.string :name
      t.text :code

      t.timestamps
    end

    add_index :repeated_answer_library_codes, :repeated_answer_task_code_id, name: 'index_code'
  end
end
