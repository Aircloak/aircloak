require 'spec_helper'
require './lib/proto/air/task_management.pb'
require './lib/protobuf_sender'

describe "ApiQueriesController" do
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

  describe "getting queries and results" do
    before(:each) do
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true
      task.ready = true
      task.save.should eq true
    end

    let (:query) { Query.create(name: "query", cluster: cluster, task: task) }
    let (:result) { Result.create(query: query, result_id: 1234) }

    describe "GET /api/queries/:id" do
      it "should return an error if the query is unknown" do
        Query.count.should eq 0
        get api_query_path(1)
        response.status.should eq 404
      end

      it "should return the list of known results" do
        query
        Query.count.should eq 1
        result
        Result.count.should eq 1
        get api_query_path(query.id)
        response.status.should eq 200
        ResultsProto.decode(response.body).result_ids.should eq [result.result_id]
      end
    end

    describe "GET /api/queries/:id/results/:id" do
      it "should return an error if the query is unknown" do
        Query.count.should eq 0
        get "/api/queries/1/results/1"
        response.status.should eq 404
      end

      it "should return an error if the result is unknown" do
        query
        Query.count.should eq 1
        get "/api/queries/#{query.id}/results/1"
        response.status.should eq 404
      end

      it "should return the the known result" do
        query
        Query.count.should eq 1
        result
        Result.count.should eq 1
        bucket = Bucket.new(result: result, label: "label", accumulated_count: 1, joiners: 2, leavers: 3)
        bucket.save.should eq true
        get "/api/queries/#{query.id}/results/#{result.result_id}"
        response.status.should eq 200
        rp = ResultProto.decode(response.body)
        rp.result_id.should eq result.result_id
        rp.properties.size.should eq 1
        rp.properties.first.label.should eq "label"
        rp.properties.first.string.should eq nil
        rp.properties.first.range.should eq nil
        rp.properties.first.joiners_leavers.should_not eq nil
        rp.properties.first.joiners_leavers.joiners.should eq 2
        rp.properties.first.joiners_leavers.leavers.should eq 3
        rp.properties.first.accumulated_count.should eq 1
      end
    end
  end

  describe "POST /api/queries" do
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

    it "should create a new query for a valid task and cluster and return its id" do
      cq = CreateQueryPB.new(cluster_id: cluster.id, main_package: "task")
      task.ready = true
      task.save.should eq true
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true

      expect {
        post api_queries_path, cq.encode.buf
        response.status.should be 200
      }.to change {Query.count}.from(0).to(1)

      response.body.should eq Query.first.id.to_s
    end
  end
end
