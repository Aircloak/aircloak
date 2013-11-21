require './lib/proto/air/management_messages.pb'
require './lib/cluster_packer'

class ClusterListsController < ApplicationController
  filter_access_to :index, require: :anon_read

  def index
    render text: ClusterPacker.package_clusters(Cluster.all).encode.buf, layout: false
  end
end
