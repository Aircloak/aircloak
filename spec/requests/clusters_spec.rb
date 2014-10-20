require 'spec_helper'
require './lib/build_manager'

describe ClustersController do
  setup :activate_authlogic

  before(:each) do
    Cluster.delete_all
    Analyst.delete_all
    Cloak.delete_all
    Build.delete_all
    Permission.delete_all
    BuildManager.stub(:send_build_request)
  end

  let(:analyst) { Analyst.create name: "test analyst" }
  let(:cloak) { Cloak.create name: "cloak", ip: "1.1.1.1" }
  let(:build) { Build.create(name: "build") }
  let(:admin_permission) { Permission.create name: "admin", description: "" }
  let(:user) {
    User.create login: "test", email: "test@example.com", password: "1234",
        password_confirmation: "1234", analyst_id: analyst.id, permissions: [admin_permission]
  }

  describe "POST /clusters" do
    it "should create a valid cluster" do
      Cluster.count.should eq 0
      log_in user

      params = {
        cluster: {name: "test cluster", build_id: build.id.to_s},
        cloak_selections: [cloak.id],
        analyst_selections: [analyst.id]
      }
      post "/clusters", params
      expect(response).to redirect_to("/clusters")
      Cluster.count.should eq 1
      Cluster.first.cloaks.should include cloak
    end
  end
end
