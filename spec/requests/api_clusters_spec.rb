require 'spec_helper'
require './lib/build_manager.rb'
require './lib/log_server_configurer'

describe "ApiClustersController" do
  before(:each) do
    LogServerConfigurer.stub(:update_config)
    BuildManager.stub(:send_build_request)
    Cluster.delete_all
    Cloak.delete_all
    Build.delete_all
    OsTag.delete_all
  end

  let (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let (:build) { Build.create(name: "build") }
  let (:os_tag) { OsTag.create(name: "OsTag", description: "Woho") }
  let (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak], os_tag: os_tag) }

  describe "setting the status of a cluster" do
    def post_status args = {}
      cs = ClusterStatusPB.new(
        status: args.delete(:status) || ClusterStatusPB::Status::ACTIVE,
        description: args.delete(:description) || "Default description"
      )
      post "/api/clusters/#{cluster.id}/status", cs.encode.buf
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
