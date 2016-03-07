class AddPeriodicCapability < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "periodic_queries",
          name: "Periodic queries",
          description: "Ability to upload, remove, and run periodic queries. Not part of release 1.0.0"
        )
      end
      dir.down do
        Capability.where(identifier: "periodic_queries").first.destroy
      end
    end
  end
end
