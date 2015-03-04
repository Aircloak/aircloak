require 'spec_helper'

describe "InfrastructureApi::TaskCodesController" do
  before(:each) do
    Cluster.delete_all
    Cloak.delete_all
    Build.delete_all
    BuildManager.stub(:send_build_request)
  end

  let!(:cloak) { Cloak.create name: "cloak", ip: "127.0.0.1" }
  let!(:build) { Build.create name: "build" }
  let!(:cluster) {
    cluster = Cluster.new name: "test cluster", build_id: build.id
    cluster.assign_cloaks [cloak]
    cluster.save.should eq true
    cluster
  }

  describe "POST /infrastructure-api/repeated_answers" do
    before(:each) do
      RaTaskCode.delete_all
      RaTaskCodeCluster.delete_all
      RaLibraryCode.delete_all
      RaLibraryCodeRaTaskCode.delete_all
    end

    let (:request) {
      {
        prefetch: "foo",
        code: "bar"
      }
    }

    it "should take a valid request" do
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(200)
    end

    it "should add it to the cluster" do
      cluster.ra_task_codes.count.should eq(0)
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(200)
      cluster.ra_task_codes.count.should eq(1)
    end

    it "requires a JSON" do
      post "/infrastructure-api/task_codes", "foo"
      response.status.should eq(403)
    end

    it "requires .prefetch" do
      request.delete(:prefetch)
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(403)
    end

    it "requires .code" do
      request.delete(:code)
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(403)
    end

    it "accepts .libraries[] with name and code" do
      request[:libraries] = [{ :name => "foo", :code => "bar" }]
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(200)
    end

    it "requires .libraries[].name" do
      request[:libraries] = [{ :code => "bar" }]
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(403)
    end

    it "requires .libraries[].code" do
      request[:libraries] = [{ :name => "foo" }]
      post "/infrastructure-api/task_codes", request.to_json
      response.status.should eq(403)
    end
  end
end
