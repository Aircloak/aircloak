class AddBasicCapabilities < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "streaming_queries",
          name: "Streaming queries",
          description: "Ability to upload, remove, and run streaming queries. Not part of release 1.0.0"
        )
        Capability.create(
          identifier: "lua_library_support",
          name: "Lua library support",
          description: "Support for bundling additional library functionality code with the task code. Used to provide a set of Aircloak stdlib's. Not part of release 1.0.0"
        )
      end
      dir.down do
        Capability.where(identifier: "streaming_queries").destroy
        Capability.where(identifier: "lua_library_support").destroy
      end
    end
  end
end
