class AddWebApiKeyToAnalyst < ActiveRecord::Migration
  def change
    reversible do |dir|
      change_table :analysts do |t|
        dir.up {
          add_column :analysts, :web_api_key, :text
          add_column :analysts, :web_api_cert, :text
        }
        dir.down {
          raise ActiveRecord::IrreversibleMigration, "Cannot undo web_api_key."
        }
      end
    end
  end
end