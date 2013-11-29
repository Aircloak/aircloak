require 'spec_helper'
require './lib/proto/air/task_management.pb'
require './lib/protobuf_sender'

describe "ApiQueriesController" do
  describe "POST /api/queries" do
    before(:each) do
      ProtobufSender.stub(:post)
      Net::HTTP.stub(:delete)
      Query.destroy_all
      Task.destroy_all
      ClusterCloak.destroy_all
      Cluster.destroy_all
      Cloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
    end

    let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }
    let! (:task) { Task.create(main_package: "task", packaged_data: "binary") }

    it "should return an error for an unknown task" do
      cq = CreateQueryPB.new(cluster_id: cluster.id, main_package: "unknown task")
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true

      Query.count.should eq 0
      post api_queries_path, cq.encode.buf
      response.status.should eq 404
      Query.count.should eq 0
    end

    it "should return an error for an invalid task" do
      cq = CreateQueryPB.new(cluster_id: cluster.id, main_package: "task")
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true

      Query.count.should eq 0
      post api_queries_path, cq.encode.buf
      response.status.should eq 400
      Query.count.should eq 0
    end

    it "should return an error for an unknown cluster" do
      cq = CreateQueryPB.new(cluster_id: cluster.id + 1, main_package: "task")
      task.ready = true
      task.save.should eq true

      Query.count.should eq 0
      post api_queries_path, cq.encode.buf
      response.status.should eq 404
      Query.count.should eq 0
    end

    it "should return an error for an invalid cluster" do
      cq = CreateQueryPB.new(cluster_id: cluster.id, main_package: "task")
      task.ready = true
      task.save.should eq true

      Query.count.should eq 0
      post api_queries_path, cq.encode.buf
      response.status.should eq 400
      Query.count.should eq 0
    end

    it "should create a new query for a valid task and cluster" do
      cq = CreateQueryPB.new(cluster_id: cluster.id, main_package: "task")
      task.ready = true
      task.save.should eq true
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true

      expect {
        post api_queries_path, cq.encode.buf
        response.status.should be 200
      }.to change {Query.count}.from(0).to(1)
    end
  end
end
