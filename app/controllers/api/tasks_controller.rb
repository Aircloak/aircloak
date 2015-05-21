require './lib/proto/air/aggregate_results.pb'
require './lib/airpub_api'

class Api::TasksController < ApplicationController
  filter_access_to [:index, :subscribe_request], require: :anon_read
  filter_access_to [:run_existing_task, :run_anon_task], require: :anon_write
  skip_before_action :verify_authenticity_token
  before_action :authenticate_api_analyst
  before_action :load_task, only: [:run_existing_task, :subscribe_request]
  respond_to :json

  def set_layout
    self.class.layout false
  end

  # GET /api/tasks
  def index
    tasks = @analyst.persistent_tasks.order(:created_at).map do |task|
      {
        token: task.token,
        name: task.name,
        cluster_name: task.cluster.name,
        type: task.type_string,
        active: task.active
      }
    end

    respond_with({success: true, tasks: tasks}, status: :ok)
  end

  # Allows users to post tasks against the web API
  # just like they would do on a cloak. The task
  # definition doesn't need to be known in the air,
  # but they get the benefit of running long running
  # tasks against the cloak (in an asynchronous fashion)
  # while having standard libraries and other equivalent
  # mechanisms available
  #
  # POST /api/tasks/run
  def run_anon_task
    body = request.body.read

    if body == ""
      description = "Your request should contain a task payload. " +
        "Please consult the API documentation for details."
      render json: {success: false, description: description}, status: :unprocessable_entity
      return
    end

    payload = JSON.parse(body)
    unless payload["cluster"] then
      description = "Please specify the cluster you want the task to run on. " +
        "You can consult the cluster API for the ID on /clusters"
      render json: {success: false, description: description}, status: :unprocessable_entity
      return
    end

    cluster_id = payload["cluster"]
    cluster = begin
      @analyst.clusters.find(cluster_id)
    rescue ActiveRecord::RecordNotFound => e
      description = "The cluster with ID #{cluster_id} does not exist." +
        "You can consult the cluster API for valid cluster ids on /clusters"
      render json: {success: false, description: description}, status: :unprocessable_entity
      return
    end

    unless payload["post_processing"] and payload["post_processing"]["code"]
      description = "The HTTP payload must include the task code to be run. " +
        "Please consult the API documentation for further information."
      render json: {success: false, description: description}, status: :unprocessable_entity
      return
    end

    unless payload["prefetch"]
      description = "The HTTP payload must include a specification of the desired " +
        "data to prefetch. Please consult the API documentation for further information."
      render json: {success: false, description: description}, status: :unprocessable_entity
      return
    end

    task = @analyst.tasks.new(
      # NOTE(sebastian): This naming scheme gives us quite a decent number of concurrent
      # tasks without naming conflicts. I think it is safe to assume that we are not
      # going to get name collisions under this implementation, with the current scale
      # and usage of the system.
      name: "Anon API task - #{Time.now.to_i}+#{rand(1000)}-#{@analyst.id}",
      cluster: cluster,
      sandbox_type: "lua",
      stored_task: false, # true for streaming or periodic tasks
      update_task: false,
      code_timestamp: Time.now,
      code: payload["post_processing"]["code"],
      prefetch: payload["prefetch"].to_json,
      one_off: true
    )

    if task.save
      pending_result = task.execute_batch_task
      result = pending_result.await_result
      render json: {success: true, result: result.to_client_hash}
    else
      description = "Could not execute the task. It failed validation. " +
        "The errors include: #{task.errors.to_a.join(", ")}"
      render json: {success: false, description: description}, status: :unprocessable_entity
    end
  end

  # Schedules a task that was defined in the web interface
  # for running on a cloak, and immediately returns.
  # POST /api/tasks/<TASK-TOKEN>/run
  def run_existing_task
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
