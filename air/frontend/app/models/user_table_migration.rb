class UserTableMigration < ActiveRecord::Base
  belongs_to :user_table, autosave: true

  validates_presence_of :user_table, :version

  def self.from_params params
    pending_migration = UserTableMigration.new
    pending_migration.migration = params[:migration]
    pending_migration.table_json = params[:table_json]
    pending_migration
  end

  def self.drop_migration table_name
    json_migration = {
      action: "drop",
      table_name: table_name
    }.to_json
    UserTableMigration.new migration: json_migration
  end
end
