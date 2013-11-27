require './lib/proto/air/query.pb'
require './lib/proto/air/query_upload.pb'

class TasksController < ApplicationController
  filter_access_to :update_task_binary, require: :anon_read
  before_action :set_task, only: [:edit, :update, :destroy]
  protect_from_forgery :except => :update_task_binary 

  # GET /tasks
  def index
    tasks = Task.all

    @ready_tasks = tasks.select { |task| task.ready }
    @not_ready_tasks = tasks.select { |task| not task.ready }
  end

  # GET /tasks/:id/edit
  def edit
  end

  # PATCH/PUT /tasks/:id
  def update
    @task.ready = true
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

  # POST /tasks/update_task_binary
  def upload_query_data
    data = request.body.read
    data_to_save = data.dup
    qd = QueryData.decode(data)
    task = Task.where(main_package: qd.main_package).first
    task = Task.new(main_package: qd.main_package) unless task
    task.packaged_data = data_to_save
    if task.save
      render text: "Thanks"
    else
      render text: "I cannot do that Dave!", status: 400
    end
    # after updating task binary we need to ensure that the clusters get the new code...
    # this update is done in the save hook of the corresponding query model, so we use that
    queries.each {|query| query.save}
  end

private
  def set_task
    @task = Task.find(params[:id])
  end

  def task_params
    params.require(:task).permit(:update_task, :payload_identifier, :system_query, :mutator)
  end
end
