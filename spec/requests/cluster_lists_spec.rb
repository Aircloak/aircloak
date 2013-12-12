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

  describe "GET /api/clusters/:id" do
    it "should provide the cluster if it is there" do
      machine = ClusterProto::MachineProto.new(
        machine_id: 1024,
        state: ClusterProto::MachineState::TO_BE_ADDED
      )
      proto = ClusterProto.new(
        timestamp: 0,
        cluster_id: 1,
        machines: [machine]
      )

      cluster = double
      Cluster.stub(:find).with("1").and_return(cluster)
      ClusterPacker.stub(:package_cluster).with(cluster).and_return(proto)

      get cluster_list_path(1)

      response.status.should be(200)
      response_proto = ClusterProto.decode(response.body)
      response_proto.should eq proto
    end

    it "should return an error if the cluster is not there" do
      get cluster_list_path(1)
      response.status.should be(404)
    end
  end
end
