require './lib/proto/air/aggregate_results.pb'

class ApiTasksController < ApplicationController
  filter_access_to [:index], require: :anon_read
  filter_access_to [:execute_as_batch_task], require: :anon_write
  skip_before_action :verify_authenticity_token
  before_action :authenticate_api_analyst, except: [:execute_as_batch_task]
  before_filter :load_task, only: [:execute_as_batch_task]
  respond_to :json

  def set_layout
    self.class.layout false
  end

  # POST /api/tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    respond_with({success: true}, {status: :created, location: nil})
  end

  # GET /api/tasks
  def index
    tasks = @analyst.tasks.order(:created_at).map do |task|
      {token: task.token, name: task.name, cluster_name: task.cluster.name, type: task.type_string}
    end

    respond_with({success: true, tasks: tasks}, status: :ok)
  end

private
  def load_task
    @task = Task.find_by_token params[:id]
    respond_with({success: false, error: "Task not found."}, {status: :unprocessable_entity, location: nil}) if @task.nil?
  end
end
