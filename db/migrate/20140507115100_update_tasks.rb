class UpdateTasks < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Bucket.delete_all
        Result.delete_all
        Query.delete_all if Object.const_defined?('Query')
        Task.delete_all
        PendingResult.delete_all

        remove_column :tasks, :ready
        remove_column :tasks, :system_task
        remove_column :tasks, :mutator
        remove_column :tasks, :packaged_data
        remove_column :tasks, :main_package
        add_column :tasks, :name, :string
        add_column :tasks, :sandbox_type, :string, :default => "lua"
        add_column :tasks, :code, :string
        add_column :tasks, :stored_task, :boolean, :default => false
        add_belongs_to :tasks, :cluster, index: true

        remove_column :results, :query_id
        add_belongs_to :results, :task, index: true

        remove_column :exception_results, :query_id
        add_belongs_to :exception_results, :task, index: true

        remove_column :pending_results, :query_id
        add_belongs_to :pending_results, :task, index: true
      end
      dir.down do
        Bucket.delete_all
        Result.delete_all
        Query.delete_all if Object.const_defined?('Query')
        Task.delete_all
        PendingResult.delete_all

        add_column :tasks, :ready, :boolean, :default => false
        add_column :tasks, :system_task, :boolean, :default => false
        add_column :tasks, :mutator, :boolean
        add_column :tasks, :packaged_data, :binary
        add_column :tasks, :main_package, :string
        remove_column :tasks, :name
        remove_column :tasks, :sandbox_type
        remove_column :tasks, :code
        remove_column :tasks, :stored_task
        remove_column :tasks, :cluster_id

        add_belongs_to :results, :query, index: true
        remove_column :results, :task_id

        add_belongs_to :exception_results, :query, index: true
        remove_column :exception_results_id, :task_id

        add_belongs_to :pending_results, :query, index: true
        remove_column :pending_results_id, :task_id
      end
    end
  end
end
