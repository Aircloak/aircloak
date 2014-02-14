require './lib/proto/air/task_management.pb'
require './lib/proto/air/aggregate_results.pb'

class ApiQueriesController < ApplicationController
  filter_access_to [:execute_as_batch_query, :create, :destroy], require: :anon_write
  filter_access_to [:show, :get_result], require: :anon_read
  skip_before_action :verify_authenticity_token
  layout false

  class ClusterNotFound < StandardError; end
  class InvalidCluster < StandardError; end
  class TaskNotFound < StandardError; end
  class InvalidTask < StandardError; end
  class QueryOrResultNotFound < StandardError; end

  # POST /api/queries
  def create
    cq = CreateQueryPB.decode(request.body.read)
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
    result = get_result_by_query params[:id], params[:result]
    send_data result.to_result_proto.encode.buf, type: "application/x-protobuf"
  rescue QueryOrResultNotFound
    render text: "Unknown query or result!", status: 404
  end

  # POST /api/queries/:id/execute_as_batch_query
  def execute_as_batch_query
    Query.find(params[:id]).execute_batch_query
    render text: "Either it succeeds or not, nobody knows!"
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

  def get_result_by_query id, result
    result = Result.where(query_id: id, result_id: result).limit(1).first
    raise QueryOrResultNotFound.new unless result
    result
  end
end
