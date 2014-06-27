require './lib/migrator'

class TablesController < ApplicationController
  before_action :set_previous_migration
  before_action :load_table, only: [:new, :create, :edit, :update, :destroy, :retry_migration]
  before_action :validate_no_pending_migrations, only: [:edit, :update, :destroy]

  def index
    @clusters = current_user.ready_clusters
    @has_tables = false
    @tables = @clusters.inject({}) do |memo, cluster|
      tables = AnalystTable.live_for_cluster(cluster, current_user.analyst)
      if tables != []
        @has_tables = true
        memo[cluster.id] = tables
      end
      memo
    end
  end

  def create
    @table = AnalystTable.from_params current_user.analyst.id, params
    migration = AnalystTableMigration.from_params params
    if Migrator.migrate @table, migration
      flash[:notice] = "Table created"
      redirect_to tables_path
    else
      set_view_params migration.table_json
      render :new
    end
  rescue
    flash[:error] = "We could not create the table changes at this time. Please try again later"
    redirect_to tables_path
  end

  def update
    migration = AnalystTableMigration.from_params params
    if Migrator.migrate @table, migration
      flash[:notice] = "Table updated"
      redirect_to tables_path
    else
      # We need to reset the table data in order to
      # preserve deleted columns
      original_table_data = JSON.parse @table.table_data
      new_table_data = JSON.parse migration.table_json
      full_table_data = original_table_data | new_table_data
      set_view_params full_table_data.to_json
      render :edit
    end

  rescue
    flash[:error] = "We could not apply the table changes at this time. Please try again later"
    redirect_to tables_path
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
    migration = AnalystTableMigration.drop_migration @table.table_name
    @table.pending_delete = true
    if Migrator.migrate @table, migration
      flash[:notice] = "Table #{@table.table_name} was dropped"
    else
      flash[:error] = "Table #{@table.table_name} could not be dropped"
    end
  rescue
    flash[:error] = "We could not drop the table at this time. Please try again later"
  ensure
    redirect_to tables_path
  end

  # This method is required for the odd circumstances where a cluster is down
  # at the point in time when an analyst wants to perform a migration.
  # It is somewhat complex, as it deals with potential creations, alterations
  # and drops.
  def retry_migration
    migration = @table.analyst_table_migrations.where(migrated: false).first
    if migration
      if Migrator.migrate @table, migration
        flash[:notice] = "Table change successfully applied"
        redirect_to tables_path
      else
        original_table_data = JSON.parse @table.table_data
        new_table_data = JSON.parse migration.table_json
        full_table_data = original_table_data | new_table_data
        set_view_params full_table_data.to_json
        @previous_migration =  migration.migration

        # Depending on if this is a new table creation
        # or a failed table edit, we need to show the
        # corresponding form
        if original_table_data.length == 0
          render :new
        else
          render :edit
        end
      end
    else
      flash[:notice] = "Already fully migrated"
      redirect_to tables_path
    end

  rescue Exception => error
    flash[:error] = "We still failed at applying the migration. Please retry later"
    redirect_to tables_path
  end

private
  def set_previous_migration
    @previous_migration = "{}"
  end

  def load_table
    if params[:id]
      @table = current_user.analyst.analyst_tables.find params[:id]
    else
      @table = current_user.analyst.analyst_tables.new
    end
    @table_data = @table.table_data
  end

  def set_view_params full_data
    @previous_migration = params[:migration]
    @table_data = full_data
  end

  def validate_no_pending_migrations
    if @table.pending_migrations?
      flash[:error] = "Cannot #{params[:action]} a table with pending migrations"
      redirect_to tables_path
    end
  end
end
