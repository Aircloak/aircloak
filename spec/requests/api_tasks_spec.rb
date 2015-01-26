require 'spec_helper'
require './lib/protobuf_sender'

describe "ApiTasksController" do
  before(:each) do
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    Task.destroy_all
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    Build.destroy_all
    BuildManager.stub(:send_build_request)
    Result.destroy_all
    Bucket.destroy_all
    Analyst.destroy_all
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  before(:each) do
    cloak.cluster_cloak.set_state :belongs_to
    cloak.cluster_cloak.save.should eq true
  end

  let (:analyst) { Analyst.create name: "test-analyst" }
  let (:token) { AnalystToken.create_api_token(analyst) }

  describe "GET /api/tasks" do
    it "returns task list" do
      tasks = (1..10).map {|i| create_task(i)}
      get "/api/tasks", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"

      json = JSON.parse(response.body)
      json["success"].should eq true
      json["tasks"].each_with_index do |t, i|
        t["token"].should eq tasks[i].token
        t["name"].should eq tasks[i].name
        t["cluster_name"].should eq tasks[i].cluster.name
        t["type"].should eq "batch"
      end
    end

    it "should require analyst" do
      get("/api/tasks", format: :json)
      response.code.should eq "401"

      get("/api/tasks", {format: :json}, {'HTTP_ANALYST_TOKEN' => "foobar"})
      response.code.should eq "401"
    end
  end

  describe "POST /api/tasks/:id/execute_as_batch_task" do
    it "should execute the task" do
      t = double
      Task.should_receive(:find_by_token).with("foobar").and_return(t)
      t.should_receive(:execute_batch_task)
      post "/api/tasks/foobar/execute_as_batch_task", {format: :json}
      response.code.should eq "201"
    end

    it "should error when task not found" do
      post "/api/tasks/unkown_token/execute_as_batch_task", {format: :json}
      response.code.should eq "422"
    end
  end

  private
    def create_task(i)
      Task.create(
        name: "task_#{i}",
        cluster: cluster,
        prefetch: "{\"bar\": \"baz\"}",
        code: "foo",
        update_task: false,
        stored_task: false,
        sandbox_type: "sandbox",
        analyst: analyst
      )
    end
end
