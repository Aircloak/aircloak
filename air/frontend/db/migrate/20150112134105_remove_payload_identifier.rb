class RemovePayloadIdentifier < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up {remove_column :tasks, :payload_identifier}
      dir.down {add_column :tasks, :payload_identifier, :string}
    end
  end
end
