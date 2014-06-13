class TasksController < ApplicationController
  before_action :set_task, only: [:edit, :update, :destroy]

  # GET /tasks
  def index
  end

  # GET /tasks/:id/edit
  def edit
  end

  # GET /tasks/new
  def new
    @task = Task.new()
  end

  # POST /tasks
  def create
    @task = Task.new(task_params)
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

private
  def set_task
    @task = Task.find(params[:id])
  end

  def task_params
    params.require(:task).permit(:name, :cluster_id, :payload_identifier, :prefetch, :code)
  end
end
