class AddTextCapability < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Capability.create(
          identifier: "postgres_text_column_support",
          name: "Support for Postgres text column type",
          description: "Ability to create tables containing columns of type text. Not available in release 1.0.0"
        )
      end
      dir.down do
        Capability.where(identifier: "postgres_text_column_support").destroy
      end
    end
  end
end
