require 'csv'
require 'airpub_api'

class TasksControllerException < Exception; end

class TasksController < ApplicationController
  filter_access_to [:execute_as_batch_task, :all_results, :latest_results,
                    :suspend, :resume, :delete, :deleted, :recover, :share, :acquire,
                    :particular_result, :pending_executions], require: :manage
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
    @private_tasks = current_user.analyst.private_tasks current_user
    @shared_tasks = current_user.analyst.shared_tasks
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
      @task.user = current_user
      describe_activity "Creating new task"
    end
  end

  # POST /tasks
  def create
    # We need to create the empty task first, so it is connected to the analyst...
    @task = current_user.analyst.tasks.new
    @task.user = current_user
    # ...only then can we validate some values (e.g. data), so we use manual mass assignment here.
    @task.attributes = task_params

    @task.sandbox_type = "lua"
    @task.update_task = false
    @task.stored_task = ([Task::STREAMING_TASK, Task::PERIODIC_TASK].include?(@task.task_type))
    @task.code_timestamp = Time.now
    @task.save_and_synchronize!
    describe_successful_activity "Successfully created a new task"
    # If we don't redirect, the flash messages get stuck
    flash[:notice] = "Task #{@task.name} was successfully created."
    redirect_to edit_task_path @task.token
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
    if @task.update(task_params) then
      @task.save_and_synchronize!
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
    begin_date = DateTime.parse(params[:begin_date])
    end_date = DateTime.parse(params[:end_date])
    @task.efficiently_delete_results begin_date, end_date
    describe_activity "Deleted #{@task.name}'s results between #{params[:begin_date]} and #{params[:end_date]}"
    flash[:notice] = "Deleted #{@task.name}'s results between #{params[:begin_date]} and #{params[:end_date]}"
    redirect_to :back
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
    if params[:begin_date].present? && params[:end_date].present? then
      begin_date = DateTime.parse(params[:begin_date])
      end_date = DateTime.parse(params[:end_date])
    else
      if @task.results.first then
        begin_date = @task.results.first.created_at
        end_date = @task.results.last.created_at
      else
        begin_date = @task.created_at
        end_date = DateTime.now.utc
      end
    end
    respond_to do |format|
      format.html do
        @raw_results = @task.results.where(:created_at => begin_date..end_date).order(created_at: :desc).
            paginate(page: params[:page], per_page: 15)
        @results = convert_results_for_client_side_rendering @raw_results # convert to json
        @results.reverse! # results are rendered in reverse order, reverse batch here to show actual order
        @results_path = all_results_task_path(@task.token)
        describe_activity "Viewed all results of task #{@task.name}", all_results_task_path(@task.token)
        # format begin/end datetimes for results filtering
        @begin_date_str = begin_date.strftime("%d/%m/%Y %H:%M:%S")
        @end_date_str = end_date.strftime("%d/%m/%Y %H:%M:%S")
      end

      format.csv do
        @raw_results = @task.results.where(:created_at => begin_date..end_date).order(created_at: :asc)
        @results = convert_results_for_client_side_rendering @raw_results # convert to json
        # format begin/end datetimes for filename creation
        begin_date_str = begin_date.strftime("%d-%m-%Y_%H-%M-%S")
        end_date_str = end_date.strftime("%d-%m-%Y_%H-%M-%S")
        filename = "#{@task.name}_from_#{begin_date_str}_to_#{end_date_str}.csv"
        response.headers['Content-Disposition'] = "attachment; filename=\"#{filename}\""
        response.headers['Content-Type'] = 'text/csv'
        describe_activity "Exported all results of task #{@task.name}", all_results_task_path(@task.token)
        render :text => results_csv
      end
    end
  end

  # GET /tasks/:id/latest_results
  def latest_results
    @results = convert_results_for_client_side_rendering(
          @task.results.includes(:exception_results)
            .order('created_at DESC')
            .limit(5)
            .reverse
        )
    @results_path = latest_results_task_path(@task.token)
    @request = AirpubApi.generate_subscribe_request "/results/#{@task.analyst.id}/#{@task.token}"
    @server_url = Rails.configuration.airpub_ws_subscribe
    @task_token = @task.token
    describe_activity "Requested latest result of task #{@task.name}", latest_results_task_path(@task.token)
  end

  # GET /tasks/:id/pending_executions
  # Return as a JSON the list of pending task executions for the particular task.
  def pending_executions
    progress = @task.pending_results.all.inject([]) do |acc, pr|
      status = pr.progress_status
      acc.push(status) unless status.nil?
      acc
    end
    render json: {success: true, progress: progress}
  end

  # GET /tasks/:id/particular_result/:timestamp
  def particular_result
    attempts = 0
    timestamp = params[:timestamp].to_i
    created_at = Time.at(timestamp / 1000, (timestamp % 1000) * 1000).utc # convert to time object
    while attempts < 5 do
      task = @task.results.where("created_at >= ?", created_at).includes(:exception_results)
      if task.size > 0
        renderable_results = convert_results_for_client_side_rendering(task)
        render json: {success: true, results: renderable_results}
        return
      else
        attempts += 1
        sleep(0.5)
      end
    end
    render json: {success: false, description: "Could not find results"}
  end

  # POST /tasks/:id/suspend
  def suspend
    @task.active = false
    @task.save_and_synchronize!
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
    @task.save_and_synchronize!
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
    @task.save_and_synchronize!
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
    @task.save_and_synchronize!
    describe_successful_activity "Deleted task #{@task.name}"
    flash[:notice] = "Deleted task #{@task.name}"
  rescue Exception => e
    describe_failed_activity "Failed at deleting task #{@task.name}"
    flash[:error] = "Could not delete task #{@task.name}. If this issue persists, please contact support\nError: #{e.message}"
  ensure
    redirect_to tasks_path
  end

  # POST /tasks/:id/share
  def share
    @task.shared = true
    @task.save
    describe_activity "Shared task #{@task.name}", share_task_path(@task.token)
    flash[:notice] = "Task #{@task.name} is now shared."
  ensure
    redirect_to :back
  end

  # POST /tasks/:id/acquire
  def acquire
    @task.shared = false
    @task.user = current_user
    @task.save
    describe_activity "Took ownership of task #{@task.name}", acquire_task_path(@task.token)
    flash[:notice] = "Task #{@task.name} is now private."
  ensure
    redirect_to :back
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
      {text: "get_user_tables()"},
      {text: "lookup", displayText: "lookup(table_name, key)"},
      {text: "to_date", displayText: "to_date(timestamp)"},
      {text: "accumulator"},
      {text: "task_time"},
      {text: "insert_row", displayText: "insert_row(table_name, row_data)"}
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
    file = CSV.generate(col_sep: ",") do |csv|
      # generate column names
      columns = ["time", "errors"] # pre-set meta-column names
      columnIndexMap = {}
      # create columns from labels and map them to header index
      @results.each do |result|
        result[:buckets].each do |bucket|
          bucket["name"] = [bucket["label"], bucket["value"]].compact.join(": ")
          if not columnIndexMap.has_key? bucket["name"]
            columnIndexMap[bucket["name"]] = columns.length
            columns.push bucket["name"]
          end
        end
      end
      csv << columns # write header

      # generate one row for each result
      @results.each do |result|
        date = Time.at(result[:published_at]/1000).strftime("%Y-%m-%d %H:%M:%S")
        errors = result[:exceptions].length > 0 ? "true" : "false"
        cells = [date, errors] + Array.new(columns.length - 2, "")
        # iterate over buckets and fill the correct cell
        result[:buckets].each do |bucket|
          index = columnIndexMap[bucket["name"]]
          cells[index] = bucket["count"]
        end
        csv << cells # write row
      end
    end
    # European versions of Excel default to semicolon as a separator instead of comma
    # (because comma is used for numbers) so we need to specify the separator explicitly
    # if we want to import the resulting file into Excel without any changes
    # this is non-standard and could break some CSV processing tools,
    # but users using other tools should be smart enough to fix this by themselves
    "sep=,\n" + file
  end
end
