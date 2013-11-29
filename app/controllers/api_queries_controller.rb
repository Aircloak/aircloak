require './lib/proto/air/task_management.pb'

class ApiQueriesController < ApplicationController
  filter_access_to :create, require: :anon_write

  class ClusterNotFound < StandardError; end
  class InvalidCluster < StandardError; end
  class TaskNotFound < StandardError; end
  class InvalidTask < StandardError; end

  def create
    cq = CreateQueryPB.decode(request.body.read)
    cluster = get_cluster cq
    task = get_task cq
    query_name = "API generated #{cq.main_package}:#{cq.cluster_id}"
    query = Query.new(name: query_name, cluster: cluster, task: task)
    if query.save
      render text: "Thank you!", layout: false
    else
      render text: "Cannot create query!", status: 400, layout: false
    end
  rescue ClusterNotFound
    render text: "I don't know that cluster!", status: 404, layout: false
  rescue InvalidCluster
    render text: "The cluster is not ready!", status: 400, layout: false
  rescue TaskNotFound
    render text: "I don't know such a task!", status: 404, layout: false
  rescue InvalidTask
    render text: "The task is not ready!", status: 400, layout: false
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

end
