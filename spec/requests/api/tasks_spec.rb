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

  describe "POST /api/tasks/<TOKEN>/run" do
    it "should require analyst" do
      post "/api/tasks/token/run"
      response.code.should eq "401"

      post "/api/tasks/token/run", "", {'HTTP_ANALYST_TOKEN' => "foobar"}
      response.code.should eq "401"
    end

    it "should require that the task is present" do
      task_token = "missing"
      Task.find_by_token(task_token).should eq nil
      post "/api/tasks/#{task_token}/run", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "404"
    end

    it "should schedule a task for running" do
      task_token = "token"
      association = double
      analyst_double = double(:analyst, tasks: association)
      AnalystToken.should_receive(:api_analyst).and_return(analyst_double)
      task = double
      association.should_receive(:find_by_token).and_return(task)
      task.should_receive(:batch_task?).and_return(true)
      task.should_receive(:execute_batch_task)
      post "/api/tasks/#{task_token}/run", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"
    end

    it "should not schedule a streaming task for running" do
      task_token = "token"
      association = double
      analyst_double = double(:analyst, tasks: association)
      AnalystToken.should_receive(:api_analyst).and_return(analyst_double)
      task = double
      association.should_receive(:find_by_token).and_return(task)
      task.should_receive(:batch_task?).and_return(false)
      post "/api/tasks/#{task_token}/run", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "400"
    end
  end

  describe "POST /api/tasks/<TOKEN>/subscribe_request" do
    it "should require analyst" do
      post "/api/tasks/token/subscribe_request"
      response.code.should eq "401"

      post "/api/tasks/token/subscribe_request", "", {'HTTP_ANALYST_TOKEN' => "foobar"}
      response.code.should eq "401"
    end

    it "should require that the task is present" do
      task_token = "missing"
      Task.find_by_token(task_token).should eq nil
      post "/api/tasks/#{task_token}/subscribe_request", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "404"
    end

    it "should return a token" do
      task_token = "token"
      association = double
      analyst_double = double(:analyst, id: 42, tasks: association)
      AnalystToken.should_receive(:api_analyst).and_return(analyst_double)
      task = double
      association.should_receive(:find_by_token).and_return(task)
      task.should_receive(:analyst).and_return(analyst)
      task.should_receive(:token).and_return(43)
      post "/api/tasks/#{task_token}/subscribe_request", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"
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
