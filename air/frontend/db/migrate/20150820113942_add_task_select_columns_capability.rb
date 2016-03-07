class AddTaskSelectColumnsCapability < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "task_select_columns",
          name: "Support for selecting columns inside the task table prefetch",
          description: "Ability to create prefetch filters with a limited number of columns."
        )
      end
      dir.down do
        Capability.where(identifier: "task_select_columns").destroy_all
      end
    end
  end
end
