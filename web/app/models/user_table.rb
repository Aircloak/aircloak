class UserTable < ActiveRecord::Base
  belongs_to :cluster
  belongs_to :analyst
  has_many :user_table_migrations, dependent: :destroy

  validates_presence_of :table_name, :cluster
  validates_uniqueness_of :table_name, scope: [:cluster_id, :analyst_id]

  def self.live_for_cluster cluster, analyst
    cluster.user_tables.where(deleted: false, analyst: analyst)
  end

  def self.from_params analyst_id, params
    existing_table = UserTable.where(table_name: params[:table_name], deleted: true, analyst_id: analyst_id).first
    table = if existing_table
      existing_table.deleted = false
      existing_table.pending_delete = false
      existing_table
    else
      UserTable.new(analyst_id: analyst_id)
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
    self.destroy if user_table_migrations.where(migrated: true).count == 0
  end

  # Returns the table data from the last
  # successful migration.
  def table_data
    latest_successful_migration = user_table_migrations
        .where(migrated: true)
        .order(created_at: :desc)
        .limit(1)
        .first
    return "[]" unless latest_successful_migration
    latest_successful_migration.table_json
  end

  def pending_migrations?
    user_table_migrations.where(migrated: false).count > 0
  end

  def row_expiry
    last_migration = user_table_migrations
        .order(created_at: :desc)
        .limit(1)
        .first

    if last_migration
      JSON.parse(last_migration.migration)["row_expiry"]
    else
      nil
    end
  end

  # Used to create example in help guides
  def generate_sample_json
    columns = JSON.parse(table_data)
    data = {}
    ["user1", "user2"].each do |user|
      data[user] ||= {}
      data[user][table_name] = []
      2.times do
        row = {}
        columns.each do |column|
          row[column["name"]] = sample_value(column["type"])
        end
        data[user][table_name] << row
      end
    end
    data
  end

  def self.sample_constraint column
    type = column["type"]
    if type =~ /varchar/
      "#{column["name"]} <> \"Carnivore\""
    elsif type == "float"
      "#{column["name"]} > 0.4"
    else
      "#{column["name"]} > 100"
    end
  end

private
  def sample_value type
    if type == "integer"
      rand(1000)
    elsif type == "float"
      rand(2) + rand
    elsif type =~ /varchar\((\d*)\)/
      string_of_size $1.to_i
    elsif type == "text"
      string_of_size rand(100).to_i
    elsif type == "boolean"
      rand(2) == 0
    end
  end

  def string_of_size size
    (0...(size)).map { (65 + rand(26)).chr }.join
  end
end
