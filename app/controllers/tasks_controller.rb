require 'csv'
require 'airpub_api'

class TasksControllerException < Exception; end

class TasksController < ApplicationController
  filter_access_to [:execute_as_batch_task, :all_results, :latest_results,
                    :suspend, :resume, :delete, :deleted, :recover], require: :manage
  before_action :load_task, except: [:index, :new, :create, :deleted]
  before_action :set_tables_json, only: [:new, :edit, :update, :create]
  before_action :set_auto_completions, only: [:new, :edit, :update, :create]
  before_action :set_task_exceptions, only: [:new, :edit, :update, :create]

  # When spawning a task from the web interface through an AJAX
  # call, this reduces the overall rendering time in rails by
  # nearly two orders of magnitude.
  respond_to :html, :json, only: :execute_as_batch_task

  # GET /tasks
  def index
    @tasks = current_user.analyst.tasks.where(:deleted => false)
    describe_activity "Browsing all tasks for #{current_user.analyst.name}"
  end

  # GET /tasks/:id/edit
  def edit
    describe_activity "Editing #{@task.name}"
  end

  # # GET /tasks/:id
  def show
    # describe_activity "Wanted to see task #{@task.name}. Showing edit form"
    redirect_to edit_task_path(@task.token)
  end

  # GET /tasks/new
  def new
    if current_user.analyst.clusters.count == 0
      describe_failed_activity "Cannot create a task without having a cluster"
      redirect_to tasks_path, flash: {error: 'Cannot create a task without having a cluster'}
    else
      @task = current_user.analyst.tasks.new
      describe_activity "Creating new task"
    end
  end

  # POST /tasks
  def create
    # We need to create the empty task first, so it is connected to
    # the analyst...
    @task = current_user.analyst.tasks.new
    # ...only then can we validate some values (e.g. data), so we use
    # manual mass assignment here.
    @task.attributes = task_params

    @task.sandbox_type = "lua"
    @task.update_task = false
    @task.stored_task = ([Task::STREAMING_TASK, Task::PERIODIC_TASK].include?(@task.task_type))
    @task.code_timestamp = Time.now
    if @task.save
      describe_successful_activity "Successfully created a new task"
      # If we don't redirect, the flash messages get stuck
      flash[:notice] = "Task #{@task.name} was successfully created."
      redirect_to edit_task_path @task.token
    else
      describe_failed_activity "Failed at creating a task"
      render action: :new
    end
  rescue Exception => e
    describe_failed_activity "Failed at creating a task"
    flash[:error] = e.message
    render action: :new
  end

  # PATCH/PUT /tasks/:id
  def update
    @task.sandbox_type = "lua"
    if @task.deleted then
      recovering = true
      @task.deleted = false
    end
    @task.code_timestamp = Time.now if @task.code != task_params[:code]
    if @task.update(task_params)
      if recovering then
        describe_successful_activity "Successfully changed and recovered task #{@task.name}"
        flash[:notice] = "Task #{@task.name} was successfully recovered"
        redirect_to tasks_path
      else
        describe_successful_activity "Successfully changed task #{@task.name}"
        flash[:notice] = 'Task was successfully saved'
        # If we don't redirect, the flash messages get stuck
        redirect_to edit_task_path @task.token
      end
    else
      describe_failed_activity "Failed at editing task #{@task.name}"
      flash[:error] = "Could not save task #{@task.name}"
      render action: :edit
    end
  rescue Exception => e
    describe_failed_activity "Failed at editing task #{@task.name}"
    flash[:error] = e.message
    render action: :edit
  end

  # DELETE /tasks/:id
  def destroy
    if @task.efficient_destroy
      describe_successful_activity "Destroyed task #{@task.name}"
      flash[:notice] = "Destroyed task #{@task.name}"
    else
      describe_failed_activity "Could not destroy task #{@task.name}"
      flash[:error] = "Could not destroy task #{@task.name}. If this persists, please contact support"
    end
    redirect_to :back
  end

  # POST /tasks/:id/delete_results
  def delete_results
    @task.efficiently_delete_results
    describe_activity "Deleted results for #{@task.name}"
    redirect_to tasks_path, notice: "Removed results for #{@task.name}"
  end

  # POST /tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    respond_to do |format|
      if @task.cluster.nil? then
        describe_failed_activity "Tried executing task #{@task.name} which doesn't have a cluster"
        error_description = "Task #{@task.name} needs a cluster to run on. Execution aborted"
        format.html do
          redirect_to tasks_path, error: error_description
        end
        format.json do
          render json: {success: false, error: error_description}
        end
      else
        @task.execute_batch_task
        describe_activity "Scheduled a batch run of task #{@task.name}"
        format.html do
          redirect_to latest_results_task_path(@task.token), notice: "Run of #{@task.name} has been initiated"
        end
        format.json do
          render json: {success: true}
        end
      end
    end
  end

  # GET /tasks/:id/all_results
  def all_results
    @raw_results = @task.results.paginate(page: params[:page], per_page: 15).order(created_at: :desc)
    @results = convert_results_for_client_side_rendering @raw_results # convert to json
    @results.reverse! # results are rendered in reverse order, reverse here to show actual order
    @results_path = all_results_task_path(@task.token)
    describe_activity "Viewed all results of task #{@task.name}", all_results_task_path(@task.token)

    respond_to do |format|
      format.html

      format.csv do
        filename = "task_results_#{@task.id}_#{Time.now.strftime("%Y%m%d%H%M")}.csv"
        response.headers['Content-Disposition'] = "attachment; filename=\"#{filename}\""
        render :text => results_csv
      end
    end
  end

  # GET /tasks/:id/latest_results
  def latest_results
    @results = convert_results_for_client_side_rendering @task.results.order('created_at DESC').limit(5).reverse
    @results_path = latest_results_task_path(@task.token)
    @request = AirpubApi.generate_subscribe_request "/results/#{@task.analyst.id}/#{@task.token}"
    @server_url = Rails.configuration.airpub_ws_subscribe
    describe_activity "Requested latest result of task #{@task.name}", latest_results_task_path(@task.token)
  end

  # POST /tasks/:id/suspend
  def suspend
    @task.active = false
    @task.save
    describe_activity "Suspended task #{@task.name}", suspend_task_path(@task.token)
    flash[:notice] = "Task #{@task.name} suspended."
  rescue Exception => e
    describe_failed_activity "Failed at suspending task #{@task.name}"
    flash[:error] = e.message
  ensure
    redirect_to :back
  end

  # POST /tasks/:id/resume
  def resume
    @task.active = true
    @task.save
    describe_activity "Resumed task #{@task.name}", resume_task_path(@task.token)
    flash[:notice] = "Task #{@task.name} resumed."
  rescue Exception => e
    describe_failed_activity "Failed at resuming task #{@task.name}"
    flash[:error] = e.message
  ensure
    redirect_to :back
  end

  # GET /tasks/deleted
  def deleted
    @tasks = current_user.analyst.tasks.where(:deleted => true)
    describe_activity "Browsing deleted tasks for #{current_user.analyst.name}"
  end

  # POST /tasks/:id/recover
  def recover
    if @task.data.blank? then
      flash[:error] = "This task is no longer valid. It needs to be fixed before it can be recovered"
      redirect_to edit_task_path(@task.token)
      return
    end

    @task.deleted = false
    @task.save
    describe_successful_activity "Recovered task #{@task.name}", recover_task_path(@task.token)
    flash[:notice] = "Task #{@task.name} was recovered."
    redirect_to tasks_path
  rescue Exception => e
    describe_failed_activity "Failed at recovering task #{@task.name}"
    flash[:error] = e.message
    redirect_to tasks_path
  end

  # POST /tasks/:id/delete
  def delete
    @task.deleted = true
    @task.purged = false
    if @task.save
      describe_successful_activity "Deleted task #{@task.name}"
      flash[:notice] = "Deleted task #{@task.name}"
    else
      describe_failed_activity "Could not delete task #{@task.name}"
      flash[:error] = "Could not delete task #{@task.name}. If this persists, please contact support"
    end
  rescue Exception => e
    describe_failed_activity "Failed at deleting task #{@task.name}"
    flash[:error] = e.message
  ensure
    redirect_to tasks_path
  end

private
  def load_task
    @task = current_user.analyst.tasks.find_by_token(params[:id])
    redirect_to tasks_path, flash: {error: 'Task not found!'} if @task.nil?
  end

  def set_tables_json
    return if require_user == false

    raise TasksControllerException.new("not a registered analyst") if current_user.analyst.nil?
    @tables_json =
      current_user.analyst.undeleted_user_tables.
          map do |table|
            {
              id: table.id,
              name: table.table_name,
              columns: JSON.parse(table.table_data),
              cluster_id: table.cluster_id
            }
          end.
          to_json
  end

  # Returns functions for the completion list in code editor.
  def set_auto_completions
    # hard-coded list of built-in functions
    completions = [
      {text: "report_property", displayText: "report_property(label, string)"},
      {text: "tables"},
      {text: "lookup", displayText: "lookup(table_name, key)"},
      {text: "to_date", displayText: "to_date(timestamp)"},
      {text: "user_id"},
      {text: "accumulator"},
      {text: "task_time"}
    ]

    # Functions declared in all library scripts
    TaskLibrary.all.each do |lib|
      lib.code.scan(/^function (?'displayText'(?'text'(\w|\.)+)\(.*\))/).each do |fun|
        completions << {text: fun[1], displayText: fun[0]}
      end
    end

    @completions = completions.to_json
  end

  def set_task_exceptions
    if @task && @task.has_exceptions?
      @task_exceptions = @task.latest_exceptions.
          map {|e| {message: e.stacktrace, count: e.count}}.
          to_json
    else
      @task_exceptions = [].to_json
    end
  end

  def task_params
    params.require(:task).permit(
          :name, :cluster_id, :data, :code, :task_type, :report_interval, :user_expire_interval, :test_data,
          :period
        )
  end

  # converts the results to a hashmap that will be converted to JSON and rendered client-side
  def convert_results_for_client_side_rendering results_raw
    results_raw.map {|result| result.to_client_hash}
  end

  def results_csv
    CSV.generate(col_sep: ";") do |csv|
      # generate column names
      columns = ["time / label", "errors"] # pre-set meta-column names
      columnIndexMap = {}
      # create columns from labels and map them to header index
      @results.each do |result|
        result[:buckets].each do |bucket|
          if not columnIndexMap.has_key? bucket[:name]
            columnIndexMap[bucket[:name]] = columns.length
            columns.push bucket[:name]
          end
        end
      end
      csv << columns # write header

      # generate one row for each result
      @results.each do |result|
        date = Time.at(result[:published_at]/1000).strftime("%Y-%m-%d %H:%M:%S")
        errors = result[:exceptions].length > 0 ? "true" : "false"
        cells = [date, errors] + Array.new(columns.length, "")
        # iterate over buckets and fill the correct cell
        result[:buckets].each do |bucket|
          index = columnIndexMap[bucket[:name]]
          cells[index] = bucket[:value]
        end
        csv << cells # write row
      end

    end
  end
end
