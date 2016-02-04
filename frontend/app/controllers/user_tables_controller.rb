require './lib/migrator'

class UserTablesController < ApplicationController
  before_action :set_previous_migration
  before_action :load_table, only: [:new, :create, :edit, :update, :destroy, :retry_migration, :clear, :show, :confirm_destroy]
  before_action :validate_no_pending_migrations, only: [:edit, :update, :destroy]
  before_action :set_create_or_edit
  filter_access_to [:confirm_destroy, :clear], require: :manage

  def index
    @clusters = current_user.ready_clusters
    @has_tables = false
    @tables = @clusters.inject({}) do |memo, cluster|
      tables = UserTable.live_for_cluster(cluster, current_user.analyst)
      if tables != []
        @has_tables = true
        memo[cluster.id] = tables
      end
      memo
    end
    describe_activity "View all user tables"
  end

  def new
    describe_activity "Creating new user table"
    render :table_editor
  end

  def show
    describe_activity "Viewing user table #{@table.table_name}"
  end

  def edit
    describe_activity "Viewing edit table page"
    render :table_editor
  end

  def create
    @table = UserTable.from_params current_user.analyst.id, params
    migration = UserTableMigration.from_params params
    if Migrator.migrate @table, migration
      flash[:notice] = "Table created"
      describe_successful_activity "Created user table", user_table_path(@table)
      redirect_to user_tables_path
    else
      describe_failed_activity "Failed at creating user table"
      @creating_new_table = true
      @previous_migration = migration.migration
      original_table_data = JSON.parse @table.table_data
      new_table_data = JSON.parse migration.table_json
      full_table_data = original_table_data | new_table_data
      set_view_params full_table_data.to_json
      render :table_editor
    end
  rescue
    describe_failed_activity "Created user table, but couldn't migrate it"
    flash[:error] = "We could not create the table changes at this time. Please try again later"
    redirect_to user_tables_path
  end

  def update
    migration = UserTableMigration.from_params params
    if Migrator.migrate @table, migration
      flash[:notice] = "Table updated"
      describe_successful_activity "Updated user table", user_table_path(@table)
      redirect_to user_tables_path
    else
      describe_failed_activity "Failed at updating", user_table_path(@table)
      # We need to reset the table data in order to
      # preserve deleted columns
      original_table_data = JSON.parse @table.table_data
      new_table_data = JSON.parse migration.table_json
      full_table_data = original_table_data | new_table_data
      set_view_params full_table_data.to_json
      render :table_editor
    end
  rescue
    describe_failed_activity "Edited user table, but couldn't migrate it"
    flash[:error] = "We could not apply the table changes at this time. Please try again later"
    redirect_to user_tables_path
  end

  # Q: Why are we not properly deleting tables, but marking them as deleted?
  # A: The reason is that the cloaks rely on version numbers to ensure
  #    table definitions stay consistent within a cluster.
  #    If a cloak was down while a table was dropped, _and_ the
  #    other cloaks deleted all knowledge of the table, then
  #    when the missing cloak came back online it would look like it
  #    had a more recent version of the table, and it would be brought
  #    back on all the other cloaks too.
  #    Therefore we record that the table has been deleted, and keep
  #    all past migrations.
  #    If a new table is created with the name of a previously dropped
  #    table, it will then continue using the previously dropped
  #    tables history.
  def destroy
    if params["table_name"] != @table.table_name then
      flash[:error] = "Entered name does not match table name!"
      redirect_to confirm_destroy_user_table_path
      return
    end
    migration = UserTableMigration.drop_migration @table.table_name
    @table.pending_delete = true
    if Migrator.migrate @table, migration
      describe_successful_activity "Dropped user table #{@table.table_name}"
      flash[:notice] = "Table #{@table.table_name} was dropped"
    else
      describe_failed_activity "Dropping user table #{@table.table_name} failed"
      flash[:error] = "Table #{@table.table_name} could not be dropped"
    end
    redirect_to user_tables_path
  rescue
    describe_failed_activity "Dropping user table migration failed for table #{@table.table_name}"
    flash[:error] = "We could not drop the table at this time. Please try again later"
    redirect_to user_tables_path
  end

  def confirm_destroy
    # we don't have a real relation tasks and user_tables so we need to search the prefetch column for the table
    @tasks = Task.all.where("prefetch LIKE ?", "%{\"table\":\"#{@table.table_name}\"%")
  end

  # This method is required for the odd circumstances where a cluster is down
  # at the point in time when an analyst wants to perform a migration.
  # It is somewhat complex, as it deals with potential creations, alterations
  # and drops.
  def retry_migration
    migration = @table.user_table_migrations.where(migrated: false).first
    if migration
      if Migrator.migrate @table, migration
        describe_successful_activity "Retried previously failed migration successfully"
        flash[:notice] = "Table change successfully applied"
        redirect_to user_tables_path
      else
        describe_failed_activity "Retried previously failed migration but it was invalid"
        original_table_data = JSON.parse @table.table_data
        new_table_data = JSON.parse migration.table_json
        full_table_data = original_table_data | new_table_data
        set_view_params full_table_data.to_json
        @previous_migration =  migration.migration

        # Depending on if this is a new table creation
        # or a failed table edit, we need to show the
        # corresponding form
        @creating_new_table = original_table_data.length == 0
        render :table_editor
      end
    else
      describe_activity "Retried migration which had already been applied"
      flash[:notice] = "Already fully migrated"
      redirect_to user_tables_path
    end
  rescue Exception
    describe_failed_activity "Re-attempted applying a migration that previously failed. It failed again."
    flash[:error] = "Creating or changing the database table still did not work. If the problem continues, please contact support."
    redirect_to user_tables_path
  end

  def clear
    result = JsonSender.request(:delete, :admin, @table.analyst, @table.cluster, "user_tables/#{@table.table_name}")

    if result["success"]
      describe_successful_activity "Cleared user table #{@table.table_name}"
      flash[:notice] = "Table #{@table.table_name} was cleared"
    else
      describe_failed_activity "Clearing user table #{@table.table_name} failed"
      flash[:error] = "Table #{@table.table_name} could not be cleared at this time. Please try again later"
    end
  ensure
    redirect_to user_tables_path
  end

private
  def set_previous_migration
    @previous_migration = "{}"
  end

  def load_table
    if params[:id]
      @table = current_user.analyst.user_tables.find params[:id]
    else
      @table = current_user.analyst.user_tables.new
    end
    @table_data = @table.table_data
  end

  def set_view_params full_data
    @previous_migration = params[:migration]
    @table_data = full_data
  end

  def set_create_or_edit
    current_action = params[:action]
    @creating_new_table = current_action == "new" or current_action == "create"
  end

  def validate_no_pending_migrations
    if @table.pending_migrations?
      flash[:error] = "Cannot #{params[:action]} a table with pending migrations"
      redirect_to user_tables_path
    end
  end
end
