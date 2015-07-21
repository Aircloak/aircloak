require 'spec_helper'
require './lib/build_manager'

describe "Api::TaskResultsController" do
  before(:each) do
    Task.destroy_all
    Analyst.destroy_all
    User.destroy_all
    Result.destroy_all
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    BuildManager.stub(:send_build_request)
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  let (:analyst) { Analyst.create name: "TestAnalyst" }
  let (:user) { User.create login: "test", email: "test@aircloak.com", analyst: analyst, password: "1234", password_confirmation: "1234" }
  let (:token) { AnalystToken.create_api_token(user) }
  let (:task) do
    t = Task.create(
      name: "task",
      cluster: cluster,
      prefetch: "{\"bar\": \"baz\"}",
      code: "foo",
      update_task: false,
      stored_task: false,
      sandbox_type: "sandbox",
      analyst: analyst,
      shared: false,
      user: user
    )
    t
  end

  describe "GET /api/tasks/:id/results" do
    it "returns an RPC specification to the backend" do
      get("/api/tasks/#{task.token}/results?from=19000101%2001:01&to=19010101%2001:01&page=2&per_page=1",
          {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token, 'request-endpoint' => "backend"})

      response.code.should eq "200"
      json = JSON.parse(response.body)
      json["rpc"].should eq "task_results_json"
      json["arguments"].should eq [task.id, 2, 1, "1900/01/01 01:01", "1901/01/01 01:01"]
    end

    it "returns an RPC specification with sane defaults" do
      get("/api/tasks/#{task.token}/results",
          {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token, 'request-endpoint' => "backend"})

      now = Time.now
      Time.stub(:now).and_return(now)
      default_end_time = now.strftime("%Y/%m/%d %H:%M")

      response.code.should eq "200"
      json = JSON.parse(response.body)
      json["rpc"].should eq "task_results_json"
      json["arguments"].should eq [task.id, 1, 10, "1970/01/01 00:01", default_end_time]
    end

    it "should require analyst" do
      get("/api/tasks/#{task.token}/results", {format: :json}, {'request-endpoint' => "backend"})
      validate_rpc_status_code 401

      get("/api/tasks/#{task.token}/results", {format: :json},
          {'HTTP_ANALYST_TOKEN' => "foobar", 'request-endpoint' => "backend"})
      validate_rpc_status_code 401
    end

    it "should require task" do
      get("/api/tasks/foobar/results", {format: :json},
          {'HTTP_ANALYST_TOKEN' => token.token, 'request-endpoint' => "backend"})
      validate_rpc_status_code 422
    end
  end

private
  def validate_rpc_status_code code
    json = JSON.parse(response.body)
    json["arguments"][1].should eq code
  end
end
