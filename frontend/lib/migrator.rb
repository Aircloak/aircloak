require './lib/json_sender'

class Migrator
  # Assembles a migration and if both the table and migration validate,
  # it will attempt to run the migration on the cluster.
  # This is a potentially very destructive operation!
  # If it succeeds, it should ALWAYS be followed by a save,
  # to ensure that the migration is recorded.
  def self.migrate table, migration
    m = Migrator.new table, migration
    m.migrate
  end

  def migrate
    @migration.user_table = @table
    set_version_number
    @migration.save and run_migration
  end

private
  def initialize table, migration
    @table = table
    @migration = migration
  end

  def run_migration
    result = JsonSender.request :post, :admin, @table.analyst, @table.cluster,
        "migrate/#{@migration.version}", {}, @migration.migration
    if result["success"]
      Rails.logger.info("Table migration successful for table: #{@table.table_name} (id: #{@table.id})")
      record_success
      true
    else
      Rails.logger.info("Table migration failed. The response was #{result}")
      record_failure
      result["error"] = "An unknown error was encountered." unless result["error"]
      @table.errors[:base] << result["error"]
      false
    end
  end

  def record_success
    ActiveRecord::Base.transaction do
      @migration.update migrated: true
      @table.update deleted: true if @table.pending_delete
    end
  end

  def record_failure
    ActiveRecord::Base.transaction do
      @migration.destroy
      # If we were unable to run the migration
      # and this was an attempt at creating the table,
      # then the table should not persist.
      @table.destroy if @table.user_table_migrations.where(migrated: true).count == 0
    end
  end

  def set_version_number
    current_migration = @table.user_table_migrations.where(migrated: true).last
    @migration.version = current_migration ? current_migration.version + 1 : 0
  end
end
