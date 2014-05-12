require './lib/proto/air/aggregate_results.pb'

class ApiTasksController < ApplicationController
  filter_access_to [:execute_as_batch_task], require: :anon_write
  filter_access_to [:show, :get_result, :get_latest_result_id], require: :anon_read
  skip_before_action :verify_authenticity_token
  layout false

  # GET /api/tasks/:id
  def show
    task = Task.find(params[:id])
    pb = ResultsPB.new(result_ids: task.results.map(&:result_id))
    send_data pb.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown task!", status: 404
  end

  # GET /api/tasks/:id/results/:result
  def get_result
    t = task_by_id params[:id]
    result = Result.where(task_id: t.id, result_id: params[:result]).limit(1).first
    raise ActiveRecord::RecordNotFound unless result
    send_data result.to_result_proto.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown task or result!", status: 404
  end

  # POST /api/tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    t = task_by_id params[:id]
    t.execute_batch_task
    render text: "Either it succeeds or not, nobody knows!"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown task!", status: 404
  end

  # GET /api/tasks/:id/latest_result_id
  # Returns the latest result id that was produced by
  # cloak-core. The results from cloak-core might arrive
  # out of order. If they do, this method will always return
  # the one produced the latest by cloak-core, which it has
  # received.
  def get_latest_result_id
    task = task_by_id params[:id]
    ids = []
    result = Result.where(task_id: task.id).order("results.result_id DESC").limit(1).first
    ids << result.result_id if result
    pb = ResultsPB.new result_ids: ids
    send_data pb.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown task", status: 404
  end

private
  def task_by_id id
    t = id.to_i == 0 ? Task.find_by_name(id) : Task.find(id)
    raise ActiveRecord::RecordNotFound unless t
    t
  end
end
