class AddUserRowExpiryCapability < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "user_row_expiry",
          name: "User row expiry",
          description: "Ability to provide expiry time for user rows."
        )
      end

      dir.down do
        Capability.where(identifier: "user_row_expiry").first.destroy
      end
    end
  end
end
