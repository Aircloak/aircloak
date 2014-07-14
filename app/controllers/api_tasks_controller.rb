require './lib/proto/air/aggregate_results.pb'

class ApiTasksController < ApplicationController
  filter_access_to [:execute_as_batch_task], require: :anon_write
  filter_access_to [:show, :get_result, :get_latest_result_id], require: :anon_read
  skip_before_action :verify_authenticity_token
  before_filter :assign_task, only: [:get_result, :execute_as_batch_task, :get_latest_result_id]

  def set_layout
    self.class.layout false
  end

  # GET /api/tasks/:id
  def show
    task = Task.find(params[:id])
    pb = ResultsPB.new(result_ids: task.results.map(&:id))
    send_data pb.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown task!", status: 404
  end

  # GET /api/tasks/:id/results/:result
  def get_result
    result = Result.find(params[:result])
    send_data result.to_result_proto.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown result for task #{@task.id}!", status: 404
  end

  # POST /api/tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    render text: "Either it succeeds or not, nobody knows!"
  end

  # GET /api/tasks/:id/latest_result_id
  # Returns the latest result id that has been received
  # by the web.
  # If results are received out of order, the result id
  # might not actually be the youngest one.
  def get_latest_result_id
    result = Result.where(task_id: @task.id).last
    ids = result ? [result.id] : []
    pb = ResultsPB.new result_ids: ids
    send_data pb.encode.buf, type: "application/x-protobuf"
  end

private
  def assign_task
    id = params[:id]
    @task = id.to_i == 0 ? Task.find_by_name(id) : Task.find(id)
  rescue ActiveRecord::RecordNotFound
  ensure
    render text: "Unknown task!", status: 404 unless @task
  end
end
