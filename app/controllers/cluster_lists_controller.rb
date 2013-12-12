require './lib/proto/air/management_messages.pb'
require './lib/cluster_packer'

class ClusterListsController < ApplicationController
  filter_access_to :index, require: :anon_read
  filter_access_to :show, require: :anon_read

  def index
    send_data ClusterPacker.package_clusters(Cluster.all).encode.buf, type: "application/x-protobuf"
  end

  def show
    send_data ClusterPacker.package_cluster(Cluster.find(params[:id])).encode.buf,
        type: "application/x-protobuf"
  rescue ActiveRecord::RecordNotFound
    render text: "The cluster is not there!", status: 404
  end
end
