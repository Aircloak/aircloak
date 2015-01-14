require 'csv'
require 'airpub_api'

class TasksControllerException < Exception; end

class TasksController < ApplicationController
  filter_access_to [:execute_as_batch_task, :all_results, :latest_results], require: :manage
  before_action :load_task, only: [:edit, :update, :destroy, :execute_as_batch_task, :all_results,
      :latest_results]
  before_action :set_tables_json, only: [:edit, :new]
  before_action :set_auto_completions, only: [:edit, :new, :create]
  before_action :set_task_exceptions, only: [:edit, :new, :create]

  # GET /tasks
  def index
    @tasks = current_user.analyst.tasks
    describe_activity "Browsing all tasks for #{current_user.analyst.name}"
  end

  # GET /tasks/:id/edit
  def edit
    describe_activity "Editing #{@task.name}"
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
    @task.stored_task = (@task.task_type == Task::STREAMING_TASK)
    if @task.save
      describe_successful_activity "Successfully created a new task"
      redirect_to tasks_path, notice: 'Task was successfully created.'
    else
      #raise @task_exceptions.inspect
      set_tables_json
      describe_failed_activity "Failed at creating a task"
      render action: 'new'
    end
  end

  # PATCH/PUT /tasks/:id
  def update
    @task.sandbox_type = "lua"
    if @task.update(task_params)
      describe_successful_activity "Successfully changed task #{@task.name}"
      redirect_to tasks_path, notice: 'Task was successfully updated.'
    else
      set_tables_json
      describe_failed_activity "Failed at editing task #{@task.name}"
      render action: 'edit'
    end
  end

  # DELETE /tasks/:id
  def destroy
    if @task.results.count > 0 then
      @task.efficient_delete
      describe_activity "Delete results for #{@task.name}"
    else
      @task.efficient_destroy
      describe_activity "Destroyed task #{@task.name}"
    end
    redirect_to tasks_path
  end

  # POST /tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    describe_activity "Scheduled a batch run of task #{@task.name}"
    redirect_to latest_results_task_path(@task), notice: "Run of #{@task.name} has been initiated"
  end

  # GET /tasks/:id/all_results
  def all_results
    @results = convert_results_for_client_side_rendering @task.results
    @results_path = all_results_task_path(@task)
    describe_activity "Viewed all results of task #{@task.name}", all_results_task_path(@task)

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
    @results_path = latest_results_task_path(@task)
    @request = AirpubApi.generate_subscribe_request "/results/#{@task.analyst.id}/#{@task.id}"
    @server_url = Rails.configuration.airpub_ws_subscribe
    describe_activity "Requested latest result of task #{@task.name}", latest_results_task_path(@task)
  end

private
  def load_task
    @task = current_user.analyst.tasks.find params[:id]
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
      {text: "user_id"}
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
          :name, :cluster_id, :data, :code, :task_type, :report_interval, :user_expire_interval
        )
  end

  # converts the results to a hashmap that will be converted to JSON and rendered client-side
  def convert_results_for_client_side_rendering results_raw
    results_raw.map { |result|
      {
        :published_at => result.created_at.utc.to_i * 1000,
        :id => result.id,
        :buckets => result.buckets.map { |bucket|
          {
            :name => bucket.display_name,
            :value => bucket.display_result
          }
        },
        :exceptions => result.exception_results.map { |exception|
          {
            :id => exception.id,
            :count => exception.count
          }
        }
      }
    }
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
