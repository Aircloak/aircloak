class UpdateExceptionResults < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        ExceptionResult.delete_all

        remove_column :exception_results, :analyst_id
        remove_column :exception_results, :task_id

        add_belongs_to :exception_results, :result, index: true
      end
      dir.down do
        ExceptionResult.delete_all

        add_belongs_to :exception_results, :analyst, index: true
        add_belongs_to :exception_results, :task, index: true

        remove_column :exception_results, :result_id
      end
    end
  end
end
