class DropOsTags < ActiveRecord::Migration
  def up
    drop_table :os_tags if ActiveRecord::Base.connection.table_exists? :os_tags
  end

  def down
    raise ActiveRecord::IrreversibleMigration
  end
end
