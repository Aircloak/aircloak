require 'spec_helper'
require './lib/proto/air/management_messages.pb'
require './lib/cluster_packer.rb'

describe ClusterListsController do
  describe "GET /api/clusters" do
    it "should provide a list of clusters" do
      machine = ClusterProto::MachineProto.new(
        machine_id: 1024,
        state: ClusterProto::MachineState::TO_BE_ADDED
      )
      cluster = ClusterProto.new(
        timestamp: 0,
        cluster_id: 1,
        machines: [machine]
      )
      proto = ClustersProto.new(clusters: [cluster])

      clusters = double
      Cluster.stub(:all).and_return(clusters)
      ClusterPacker.stub(:package_clusters).with(clusters).and_return(proto)

      get cluster_lists_path

      response.status.should be(200)
      response_proto = ClustersProto.decode(response.body)
      response_proto.should eq proto
    end
  end
end
