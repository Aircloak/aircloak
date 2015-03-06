require './lib/proto/air/aggregate_results.pb'
require './lib/airpub_api'

class Api::TasksController < ApplicationController
  filter_access_to [:index, :subscribe_request], require: :anon_read
  filter_access_to [:run], require: :anon_write
  skip_before_action :verify_authenticity_token
  before_action :authenticate_api_analyst
  before_action :load_task, only: [:run, :subscribe_request]
  respond_to :json

  def set_layout
    self.class.layout false
  end

  # GET /api/tasks
  def index
    tasks = @analyst.tasks.order(:created_at).map do |task|
      {token: task.token, name: task.name, cluster_name: task.cluster.name, type: task.type_string}
    end

    respond_with({success: true, tasks: tasks}, status: :ok)
  end

  # POST /api/tasks/<TASK-TOKEN>/run
  # Note(sebastian): This is an undocumented API endpoint created for Michal at Telefonica.
  # I am not yet sure if this is something we want to support long term, so I don't
  # want to make it publicly known and used.
  def run
    if @task.batch_task?
      @task.execute_batch_task
      render json: {success: true}, status: :ok
    else
      render json: {success: false, description: "Only batch queries can be schedule for execution"},
          status: :bad_request
    end
  end

  # POST /api/tasks/<TASK-TOKEN>/subscribe_request
  def subscribe_request
    render json: {
          success: true,
          token: AirpubApi.generate_subscribe_request("/results/#{@task.analyst.id}/#{@task.token}")
        }
  end

  private
    def load_task
      @task = @analyst.tasks.find_by_token params[:id]
      if !@task
        render json: {success: false, description: "Unknown task"}, status: :not_found
      end
    end
end
