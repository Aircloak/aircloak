class AddAdminAndTaskRunnerKeysToAnalyst < ActiveRecord::Migration
  def change
    reversible do |dir|
      change_table :analysts do |t|
        dir.up {
          add_column :analysts, :admin_key, :text
          add_column :analysts, :admin_cert, :text

          add_column :analysts, :task_runner_key, :text
          add_column :analysts, :task_runner_cert, :text

          # We do not invalidate existing inquirer keys
          # as it would break existing setups for older clusters.
          # But we remove them, meaning it will not be possible
          # to perform additional changes to existing cluster,
          # other than running with them as they are.
          remove_column :analysts, :inquirer_key
          remove_column :analysts, :inquirer_cert

          Analyst.all.each do |analyst|
            analyst.send :create_admin_token
            analyst.send :create_task_runner_token
            analyst.save
          end
        }
        dir.down   {
          raise "Cannot undo inquirer key removal as implementation does not support it anymore"
        }
      end
    end
  end
end
