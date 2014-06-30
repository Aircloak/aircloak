class TasksController < ApplicationController
  filter_access_to :execute_as_batch_task, require: :manage
  before_filter :load_task, only: [:edit, :update, :destroy, :execute_as_batch_task]

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
    @task = current_user.analyst.tasks.new task_params
    @task.sandbox_type = "lua"
    @task.update_task = false
    @task.stored_task = false
    if @task.save
      redirect_to tasks_path, notice: 'Task was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /tasks/:id
  def update
    @task.sandbox_type = "lua"
    if @task.update(task_params)
      redirect_to tasks_path, notice: 'Task was successfully updated.'
    else
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
    redirect_to result_path(@task)
  end

private
  def load_task
    @task = current_user.analyst.tasks.find params[:id]
  end

  def task_params
    params.require(:task).permit(:name, :cluster_id, :payload_identifier, :prefetch, :code)
  end
end
