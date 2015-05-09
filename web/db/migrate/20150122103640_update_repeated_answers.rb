class UpdateRepeatedAnswers < ActiveRecord::Migration
  def change
    # DANGER: we just throw away all known repeated answer reports (Only use this migration if you know what
    # you are doing)
    # IMPORTANT NOTE: You should never apply migrations without reading them in a production environment!
    RepeatedAnswer.delete_all

    reversible do |dir|
      dir.up do
        drop_table :repeated_answer_task_codes
        drop_table :repeated_answer_library_codes
      end

      dir.down do
        create_table :repeated_answer_task_codes do |t|
          t.belongs_to :repeated_answer

          t.text :prefetch
          t.text :code

          t.timestamps
        end

        create_table :repeated_answer_library_codes do |t|
          t.belongs_to :repeated_answer_task_code

          t.string :name
          t.text :code
          
          t.timestamps
        end
      end
    end
  end
end
