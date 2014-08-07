require 'csv'

class TasksControllerException < Exception; end

class TasksController < ApplicationController
  filter_access_to [:execute_as_batch_task, :all_results, :latest_results], require: :manage
  before_action :load_task, only: [:edit, :update, :destroy, :execute_as_batch_task, :all_results,
      :latest_results]
  before_action :set_tables_json, only: [:edit, :new]

  # GET /tasks
  def index
    @tasks = current_user.analyst.tasks
  end

  # GET /tasks/:id/edit
  def edit
  end

  # GET /tasks/new
  def new
    @task = current_user.analyst.tasks.new
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
    @task.stored_task = false
    if @task.save
      redirect_to tasks_path, notice: 'Task was successfully created.'
    else
      set_tables_json
      render action: 'new'
    end
  end

  # PATCH/PUT /tasks/:id
  def update
    @task.sandbox_type = "lua"
    if @task.update(task_params)
      redirect_to tasks_path, notice: 'Task was successfully updated.'
    else
      set_tables_json
      render action: 'edit'
    end
  end

  # DELETE /tasks/:id
  def destroy
    @task.efficient_delete
    redirect_to tasks_path
  end

  # POST /tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    redirect_to latest_results_task_path(@task), notice: "Run of #{@task.name} has been initiated"
  end

  # GET /tasks/:id/all_results
  def all_results
    @results = @task.results
    @results_path = all_results_task_path(@task)
    render_results
  end

  # GET /tasks/:id/latest_results
  def latest_results
    @results = @task.results.order('created_at DESC').limit(3).reverse
    @results_path = latest_results_task_path(@task)
    render_results
  end

private
  def load_task
    @task = current_user.analyst.tasks.find params[:id]
  end

  def set_tables_json
    return if require_user == false

    raise TasksControllerException.new("not a registered analyst") if current_user.analyst.nil?
    @tables_json =
      current_user.analyst.analyst_tables.
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

  def task_params
    params.require(:task).permit(:name, :cluster_id, :payload_identifier, :data, :code)
  end

  def render_results
    @result_set = @task.result_set(@results)
    respond_to do |format|
      format.html

      format.csv do
        filename = "task_results_#{@task.id}_#{Time.now.strftime("%Y%m%d%H%M")}.csv"
        response.headers['Content-Disposition'] = "attachment; filename=\"#{filename}\""
        render :text => results_csv
      end
    end
  end

  def results_csv
    CSV.generate(col_sep: ";") do |csv|
      csv << (@result_set.keys + ["created at"])
      @task.results.each do |result|
        csv << (
          @result_set.map {|name, bucket| bucket[result.id] || ""} +
          [result.created_at.utc.strftime("%Y-%m-%d %H:%M:%S")]
        )
      end
    end
  end
end
