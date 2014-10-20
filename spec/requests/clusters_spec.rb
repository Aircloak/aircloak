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
    log_in user
  end

  let(:analyst) { Analyst.create name: "test analyst" }
  let(:cloak) { Cloak.create name: "cloak", ip: "1.1.1.1" }
  let(:cloak2) { Cloak.create name: "cloak2", ip: "1.1.1.2" }
  let(:build) { Build.create name: "build" }
  let(:build2) {
    build = Build.new name: "build2"
    build.save validate: false
    build
  }
  let(:admin_permission) { Permission.create name: "admin", description: "" }
  let(:user) {
    User.create login: "test", email: "test@example.com", password: "1234",
        password_confirmation: "1234", analyst_id: analyst.id, permissions: [admin_permission]
  }
  let(:cluster) {
    cluster = Cluster.new name: "test cluster", build_id: build.id
    cluster.assign_cloaks [cloak]
    cluster.save.should eq true
    cluster
  }
  let(:params) {
    {
      cluster: {name: "test cluster", build_id: build.id.to_s},
      cloak_selections: [cloak.id],
      analyst_selections: [analyst.id]
    }
  }

  describe "POST /clusters" do
    it "should create a valid cluster" do
      Cluster.count.should eq 0
      post "/clusters", params
      expect(response).to redirect_to("/clusters")
      Cluster.count.should eq 1
      Cluster.first.cloaks.should include cloak
    end
  end

  describe "UPDATE /clusters" do
    it "should update a clusters build" do
      cluster.build_id.should eq build.id
      Cluster.count.should eq 1
      cloak.cluster_cloak.set_state :belongs_to
      cloak.save
      params[:cluster][:build_id] = build2.id
      put "/clusters/#{cluster.id}", params
      expect(response).to redirect_to("/clusters")
      Cluster.count.should eq 1
      cluster.reload.build_id.should eq build2.id
      cloak.reload.cluster_cloak.state.should eq :to_be_upgraded
    end

    it "should mark changed clusters" do
      cluster.build_id.should eq build.id
      Cluster.count.should eq 1
      cloak.cluster_cloak.set_state :belongs_to
      cloak.save
      params[:cloak_selections] << cloak2.id
      put "/clusters/#{cluster.id}", params
      expect(response).to redirect_to("/clusters")
      Cluster.count.should eq 1
      cluster.reload.cloaks.count.should eq 2
      cloak.reload.cluster_cloak.state.should eq :belongs_to
      cloak2.reload.cluster_cloak.state.should eq :to_be_added
    end
  end
end
