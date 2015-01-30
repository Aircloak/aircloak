require 'spec_helper'
require './lib/build_manager.rb'

describe "ApiClustersController" do
  before(:each) do
    BuildManager.stub(:send_build_request)
    Cluster.delete_all
    Cloak.delete_all
    Build.delete_all
  end

  let (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let (:build) { Build.create(name: "build") }
  let (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  describe "setting the status of a cluster" do
    def post_status args = {}
      cs = ClusterStatusPB.new(
        status: args.delete(:status) || ClusterStatusPB::Status::ACTIVE,
        description: args.delete(:description) || "Default description"
      )
      post "/infrastructure-api/clusters/#{cluster.id}/status", cs.encode.buf
      response.status.should eq 200
    end

    it "should set status" do
      post_status status: ClusterStatusPB::Status::ACTIVE
      cluster.reload.status.should eq :active
      post_status status: ClusterStatusPB::Status::IN_SERVICE
      cluster.reload.status.should eq :in_service
      post_status status: ClusterStatusPB::Status::INACTIVE
      cluster.reload.status.should eq :inactive
    end

    it "should set status description" do
      post_status status: ClusterStatusPB::Status::ACTIVE
      cluster.reload.status_description.should eq ""
      post_status status: ClusterStatusPB::Status::IN_SERVICE
      cluster.reload.status_description.should_not eq ""
      post_status status: ClusterStatusPB::Status::INACTIVE
      cluster.reload.status_description.should_not eq ""
    end
  end
end
