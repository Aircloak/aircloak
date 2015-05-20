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

  describe "POST /api/tasks/run" do
    it "should require analyst" do
      post "/api/tasks/run"
      response.code.should eq "401"

      post "/api/tasks/run", "", {'HTTP_ANALYST_TOKEN' => "foobar"}
      response.code.should eq "401"
    end

    def payload
      {
        prefetch: {},
        post_processing: {code: "business logic"},
        cluster: cluster.id
      }
    end

    it "requires a cluster" do
      payload_without_cluster = payload
      payload_without_cluster.delete(:cluster)
      post "/api/tasks/run", payload_without_cluster.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /cluster/i).should_not eq nil
    end

    it "requires a cluster" do
      # The cluster doesn't belong to the analyst
      post "/api/tasks/run", payload.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /cluster/i).should_not eq nil
      (body["description"] =~ /does not exist/i).should_not eq nil
    end

    it "requires content payload" do
      post "/api/tasks/run", "", {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /payload/i).should_not eq nil
    end

    it "requires a code segment" do
      cluster.analysts << analyst
      payload_without_code = payload
      payload_without_code[:post_processing].delete(:code)
      post "/api/tasks/run", payload_without_code.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /code/i).should_not eq nil

      payload_without_code.delete(:post_processing)
      post "/api/tasks/run", payload_without_code.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /code/i).should_not eq nil
    end

    it "requires a prefetch clause" do
      cluster.analysts << analyst
      payload_without_prefetch = payload
      payload_without_prefetch.delete(:prefetch)
      post "/api/tasks/run", payload_without_prefetch.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /prefetch/i).should_not eq nil
    end

    it "should return validation errors" do
      cluster.analysts << analyst
      task = double(:task)
      Task.should_receive(:new).and_return(task)
      task.should_receive(:save).and_return(false)
      errors = double(to_a: ["Validation failed"])
      task.should_receive(:errors).and_return(errors)

      post "/api/tasks/run", payload.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "422"
      body = JSON.parse(response.body)
      body["success"].should eq false
      (body["description"] =~ /validation failed/i).should_not eq nil
    end

    it "creates and runs a task and returns the results" do
      cluster.analysts << analyst
      task = double(:task)
      Task.should_receive(:new).and_return(task)
      task.should_receive(:save).and_return(true)
      result = double(:result, to_client_hash: {buckets: "and stuff"})
      pending_result = double(:pending_result, await_result: result)
      task.should_receive(:execute_batch_task).and_return(pending_result)
      post "/api/tasks/run", payload.to_json, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"
      body = JSON.parse(response.body)
      body["success"].should eq true
      body["result"].should_not eq nil
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
