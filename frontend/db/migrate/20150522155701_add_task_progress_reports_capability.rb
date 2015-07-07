class AddTaskProgressReportsCapability < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "task_progress_reports",
          name: "Task progress reports",
          description: "Ability to query the progress of batch tasks."
        )
      end

      dir.down do
        Capability.where(identifier: "task_progress_reports").first.destroy
      end
    end
  end
end
