require './lib/proto/air/task_management.pb'
require './lib/proto/air/aggregate_results.pb'

class ApiQueriesController < ApplicationController
  filter_access_to [:execute_as_batch_query, :execute_named_batch_query, :create, :destroy], require: :anon_write
  filter_access_to [:show, :get_result, :get_latest_result_id], require: :anon_read
  skip_before_action :verify_authenticity_token
  layout false

  class ClusterNotFound < StandardError; end
  class InvalidCluster < StandardError; end
  class TaskNotFound < StandardError; end
  class InvalidTask < StandardError; end

  # POST /api/queries
  def create
    cq = CreateQueryPB.decode(request.raw_post)
    cluster = get_cluster cq
    task = get_task cq
    query_name = "API generated #{cq.main_package}:#{cq.cluster_id}"
    query = Query.new(name: query_name, cluster: cluster, task: task)
    if query.save
      render text: "#{query.id}"
    else
      render text: "Cannot create query!", status: 400
    end
  rescue ClusterNotFound
    render text: "I don't know that cluster!", status: 404
  rescue InvalidCluster
    render text: "The cluster is not ready!", status: 400
  rescue TaskNotFound
    render text: "I don't know such a task!", status: 404
  rescue InvalidTask
    render text: "The task is not ready!", status: 400
  end

  # GET /api/queries/:id
  def show
    query = Query.find(params[:id])
    pb = ResultsProto.new(result_ids: query.results.map(&:result_id))
    send_data pb.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown query!", status: 404
  end

  # DELETE /api/queries/:id
  def destroy
    query = Query.find(params[:id])
    query.destroy
    render text: "query #{params[:id]} destroyed"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown query!", status: 404
  end

  # GET /api/queries/:id/results/:result
  def get_result
    q = query_by_id params[:id]
    result = Result.where(query_id: q.id, result_id: params[:result]).limit(1).first
    raise ActiveRecord::RecordNotFound unless result
    send_data result.to_result_proto.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown query or result!", status: 404
  end

  # POST /api/queries/:id/execute_as_batch_query
  def execute_as_batch_query
    q = query_by_id params[:id]
    q.execute_batch_query
    render text: "Either it succeeds or not, nobody knows!"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown query!", status: 404
  end

  # POST /api/queries/execute_named_batch_query/:name
  def execute_named_batch_query
    q = Query.find_by_name(params[:name])
    if q
      q.execute_batch_query
      render text: "Executing the query named #{params[:name]}"
    else
      render text: "Unknown query!", status: 404
    end
  end

  # GET /api/queries/:id/latest_result_id
  # Returns the latest result id that was produced by
  # cloak-core. The results from cloak-core might arrive
  # out of order. If they do, this method will always return
  # the one produced the latest by cloak-core, which it has
  # received.
  def get_latest_result_id
    query = query_by_id params[:id]
    ids = []
    result = Result.where(query_id: query.id).order("results.result_id DESC").limit(1).first
    ids << result.result_id if result
    pb = ResultsProto.new result_ids: ids
    send_data pb.encode.buf, type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "Unknown query", status: 404
  end

private
  def get_cluster cq
    cluster = Cluster.find(cq.cluster_id)
    raise InvalidCluster.new unless cluster.ready?
    cluster
  rescue ActiveRecord::RecordNotFound
    raise ClusterNotFound.new
  end

  def get_task cq
    task = Task.find_by_main_package(cq.main_package)
    raise TaskNotFound.new unless task
    raise InvalidTask.new unless task.ready?
    task
  end

  def query_by_id id
    q = id.to_i == 0 ? Query.find_by_name(id) : Query.find(id)
    raise ActiveRecord::RecordNotFound unless q
    q
  end
end
