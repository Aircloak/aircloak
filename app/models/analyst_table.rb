class AnalystTable < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :analyst
  has_many :analyst_table_migrations, dependent: :destroy

  validates_presence_of :table_name, :cluster

  # FIXME(#256): As soon as we have proper analyst id's in the system,
  # we should ensure that the uniqueness constraint is on the
  # (table_name, analyst_id) pair instead of
  # (table_name, cluster_id)
  validates_uniqueness_of :table_name, scope: :cluster_id

  def self.live_for_cluster cluster, analyst
    cluster.analyst_tables.where(deleted: false, analyst: analyst)
  end

  def self.from_params params
    existing_table = AnalystTable.where(table_name: params[:table_name], deleted: true).first
    table = if existing_table
      existing_table.deleted = false
      existing_table.pending_delete = false
      existing_table
    else
      AnalystTable.new
    end

    table.cluster = Cluster.find params[:cluster_id] unless table.cluster
    table.table_name = params[:table_name] unless table.table_name

    table
  end

  def record_successful_migration
    update deleted: true if pending_delete
  end

  def record_failed_migration
    # If we were unable to run the migration
    # and this was an attempt at creating the table,
    # then the table should not persist.
    self.destroy if analyst_table_migrations.where(migrated: true).count == 0
  end

  # Returns the table data from the last
  # successful migration.
  def table_data
    latest_successful_migration = analyst_table_migrations
        .where(migrated: true)
        .order(created_at: :desc)
        .limit(1)
        .first
    return "[]" unless latest_successful_migration
    latest_successful_migration.table_json
  end

  def pending_migrations?
    analyst_table_migrations.where(migrated: false).count > 0
  end
end
