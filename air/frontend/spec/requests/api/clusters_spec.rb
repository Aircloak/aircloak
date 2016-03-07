require 'spec_helper'
require 'pry'

describe "ClustersController" do
  before(:each) do
    Cluster.delete_all
    Cloak.delete_all
    Build.delete_all
    BuildManager.stub(:send_build_request)
    Result.delete_all
    Analyst.delete_all
    User.delete_all
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  before(:each) do
    cloak.cluster_cloak.set_state :belongs_to
    cloak.cluster_cloak.save.should eq true
  end

  let (:analyst) { Analyst.create name: "test-analyst" }
  let (:user) { User.create login: "test", email: "test@aircloak.com", analyst: analyst, password: "1234", password_confirmation: "1234" }
  let (:token) { UserToken.create_api_token(user) }

  describe "GET /api/clusters" do
    it "returns empty cluster list if no clusters" do
      get "/api/clusters", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"

      json = JSON.parse(response.body)
      json["success"].should eq true
      json["clusters"].length.should eq 0
    end

    it "returns all clusters the analyst has access to" do
      cluster.analysts << analyst

      get "/api/clusters", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token}
      response.code.should eq "200"

      json = JSON.parse(response.body)
      json["success"].should eq true
      cluster_json = json["clusters"].first
      cluster_json["id"].should eq cluster.id
      cluster_json["name"].should eq cluster.name
    end

    it "should require analyst" do
      get("/api/clusters", format: :json)
      response.code.should eq "401"

      get("/api/clusters", {format: :json}, {'HTTP_ANALYST_TOKEN' => "foobar"})
      response.code.should eq "401"
    end
  end
end
