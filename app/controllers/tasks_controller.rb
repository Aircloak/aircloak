class TasksControllerException < Exception; end

class TasksController < ApplicationController
  filter_access_to :execute_as_batch_task, require: :manage
  before_action :load_task, only: [:edit, :update, :destroy, :execute_as_batch_task]
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
    @task.destroy
    redirect_to tasks_path
  end

  # POST /tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    redirect_to result_path(@task), notice: "Run of #{@task.name} has been initiated"
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
end
