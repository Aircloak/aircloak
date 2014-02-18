require 'spec_helper'
require './lib/proto/air/management_messages.pb'
require './lib/cluster_packer.rb'

describe ClusterListsController do
  describe "GET /api/clusters" do
    it "should provide a list of clusters" do
      machine = ClusterPB::MemberPB.new(
        machine_id: 1024,
        state: ClusterPB::MachineState::TO_BE_ADDED
      )
      cluster = ClusterPB.new(
        timestamp: 0,
        id: 1,
        machines: [machine]
      )
      proto = ClustersPB.new(clusters: [cluster])

      clusters = double
      Cluster.stub(:all).and_return(clusters)
      ClusterPacker.stub(:package_clusters).with(clusters).and_return(proto)

      get "/api/clusters"

      response.status.should be(200)
      response_proto = ClustersPB.decode(response.body)
      response_proto.should eq proto
    end
  end

  describe "GET /api/clusters/:id" do
    it "should provide the cluster if it is there" do
      machine = ClusterPB::MemberPB.new(
        id: 1024,
        state: ClusterPB::MachineState::TO_BE_ADDED
      )
      proto = ClusterPB.new(
        timestamp: 0,
        id: 1,
        machines: [machine]
      )

      cluster = double
      Cluster.stub(:find).with("1").and_return(cluster)
      ClusterPacker.stub(:package_cluster).with(cluster).and_return(proto)

      get "/api/clusters/1"

      response.status.should be(200)
      response_proto = ClusterPB.decode(response.body)
      response_proto.should eq proto
    end

    it "should return an error if the cluster is not there" do
      get "/api/clusters/1"
      response.status.should be(404)
    end
  end
end
