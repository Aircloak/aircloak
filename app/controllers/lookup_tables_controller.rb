require './lib/json_sender'

class LookupTablesController < ApplicationController
  def index
    @clusters = current_user.ready_clusters
    @tables = current_user.analyst.lookup_tables.where(deleted: false).includes(:cluster).
        inject({}) do |memo, table|
          memo[table.cluster.id] ||= []
          memo[table.cluster.id] << table
          memo
        end
    describe_activity "View all lookup tables"
  end

  def new
    @table = current_user.analyst.lookup_tables.new
    describe_activity "Creating new lookup table"
  end

  def edit
    @table = current_user.analyst.lookup_tables.find params[:id]
    describe_activity "Editing lookup table", lookup_table_path(@table)
  end

  def create
    @table = LookupTable.new(
          analyst_id: current_user.analyst.id,
          cluster_id: params[:cluster_id],
          table_name: params[:table_name]
        )
    @table.upload_data = params[:upload_data]
    if @table.valid?
      result = upload_table
      if result["success"]
        @table.save
        describe_successful_activity "Created lookup table", lookup_table_path(@table)
        flash[:notice] = "Lookup table created."
        redirect_to lookup_tables_path
      else
        describe_failed_activity "Cloak error while creating lookup table"
        flash[:error] = result["error"] || "Table creation failed."
        render :new
      end
    else
      describe_failed_activity "Invalid data provided while creating lookup table"
      render :new
    end
  end

  def update
    @table = current_user.analyst.lookup_tables.find params[:id]
    @table.upload_data = params[:upload_data]
    if @table.valid?
      result = upload_table
      if result["success"]
        @table.save
        describe_successful_activity "Updated lookup table", lookup_table_path(@table)
        flash[:notice] = "Lookup table data updated."
        redirect_to lookup_tables_path
      else
        describe_failed_activity "Cloak error while updating lookup table"
        flash[:error] = result["error"] || "Table update failed. The previous table data is still available."
        render :edit
      end
    else
      describe_failed_activity "Invalid data provided while updating lookup table"
      render :edit
    end
  end

  def destroy
    @table = current_user.analyst.lookup_tables.find params[:id]
    result = remove_table
    if result["success"]
      @table.update deleted: true
      describe_successful_activity "Removed lookup table", lookup_table_path(@table)
      flash[:notice] = "Lookup table removed."
      redirect_to lookup_tables_path
    else
      describe_failed_activity "Cloak error while removing lookup table"
      flash[:error] = result["error"] || "Table removal failed. The previous table data is still available."
      redirect_to lookup_tables_path
    end
  end

private
  def upload_table
    JsonSender.post(current_user.analyst, @table.cluster, "lookup/upload", @table.upload_data,
          table: @table.table_name)
  end

  def remove_table
    JsonSender.post(current_user.analyst, @table.cluster, "lookup/remove", '',
          table: @table.table_name)
  end
end