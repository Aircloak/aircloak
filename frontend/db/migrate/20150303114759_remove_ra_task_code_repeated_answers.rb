class RemoveRaTaskCodeRepeatedAnswers < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        # Before destroying all repeated answers, copy over all task codes to the clusters and attach all
        # repeated answers to the corresponding cluster.
        if ActiveRecord::Base.connection.table_exists? :ra_task_code_repeated_answers
          sql = "SELECT repeated_answer_id, ra_task_code_id FROM ra_task_code_repeated_answers"
          ActiveRecord::Base.connection.execute(sql).each do |table_row|
            ra_task_code = RaTaskCode.find(table_row["ra_task_code_id"])
            repeated_answer = RepeatedAnswer.find(table_row["repeated_answer_id"])
            repeated_answer.analyst.clusters.each do |cluster|
              cluster.add_task_code ra_task_code
              cluster.repeated_answers << repeated_answer
            end
          end

          drop_table :ra_task_code_repeated_answers
        end
      end

      dir.down do
        raise ActiveRecord::IrreversibleMigration
      end
    end
  end
end
